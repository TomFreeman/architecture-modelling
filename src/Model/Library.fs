module Model
open System

type UnitType =
    | ExternalService
    | InternalService
    | Team
    | Individual

type ServiceResult =
    | Success of TimeSpan
    | Failure of TimeSpan

[<CustomEquality; CustomComparison>]
type ReliabilityProfile = {
    shorthand: string
    works: unit -> ServiceResult
}
    with
        override this.Equals(other) =
            match other with
            | :? ReliabilityProfile as o -> this.shorthand = o.shorthand
            | _ -> false

        override this.GetHashCode() =
            this.shorthand.GetHashCode()

        interface System.IComparable with
            override this.CompareTo(other) =
                match other with
                | :? ReliabilityProfile as o -> this.shorthand.CompareTo(o.shorthand)
                | _ -> 1

let perfectUptime = {
    shorthand = "perfect"
    works = fun () -> Success (TimeSpan.FromMilliseconds(1))
}


let randomUptimeProfile uptime =
    let rand = new Random()
    {
        shorthand = sprintf "random %f" uptime
        works = fun () ->
                    if rand.NextDouble() > uptime then
                        Failure(TimeSpan.FromMilliseconds(1))
                    else
                        Success(TimeSpan.FromMilliseconds(1))
    }

type relationships =
    | Requires of Component
    | BenefitsFrom of Component
    | ComposedOf of Component
    | ResponsibilityOf of Component
and Link = {
    relationship: relationships
    metadata: Map<string, string> option
}
and [<CustomEquality; CustomComparison>] Component = {
            name: string
            links: Link array
            serviceType: UnitType
            reliabilityProfile: ReliabilityProfile
            metadata: Map<string, string> option
        }
    with
        override this.Equals(other) =
            match other with
            | :? Component as o ->  this.name = o.name &&
                                                this.serviceType = o.serviceType &&
                                                this.reliabilityProfile = o.reliabilityProfile &&
                                                this.metadata = o.metadata &&
                                                this.links = o.links

            | _ -> false

        override this.GetHashCode() =
            this.name.GetHashCode() +
            this.serviceType.GetHashCode() +
            this.reliabilityProfile.GetHashCode() +
            this.metadata.GetHashCode() +
            this.links.GetHashCode()

        interface System.IComparable with
            override this.CompareTo(other) =
                match other with
                | :? Component as o -> this.name.CompareTo(o.name)
                | _ -> 1
type ServiceLevel =
    | Working
    | Unavailable
    | Degraded

let plain relationship =
    { relationship = relationship; metadata = None }

let noMetadata (relationships: relationships seq) =
    relationships
    |> Seq.map (fun (relationship) -> plain relationship)
    |> Seq.toArray

let buildTeam teamname names metadata =
    let individuals = names
                    |> Array.map (fun (name) ->
                        plain(ComposedOf(
                        {name = name;
                        links = [||];
                        serviceType = Individual;
                        reliabilityProfile = perfectUptime;
                        metadata = None})))
    {
        name = teamname
        links = individuals
        serviceType = Team
        reliabilityProfile = perfectUptime
        metadata = metadata
    }


let (| Working | Unavailable | ) (service: Component) =
    match service.reliabilityProfile.works() with
    | Success(_) -> Working
    | _ -> Unavailable

let mitigatedBy strategy (service: Component) =
    {service with reliabilityProfile = strategy(service.reliabilityProfile)}

let rec walkDependencies (visited: Set<string>) currentState (dependencies: Link array) =

    if dependencies.Length = 0 then
        currentState
    else
        let head = dependencies.[0]
        let deps = dependencies.[1..dependencies.Length - 1]

        match head.relationship with
        | Requires(s)
        | ComposedOf(s) ->
                            if visited.Contains(s.name) then
                                currentState
                            else
                                visited.Add(s.name) |> ignore
                                match s with
                                | Working -> walkDependencies visited currentState (Array.concat([deps; s.links]))
                                | Unavailable -> Unavailable
        | BenefitsFrom(s) ->
                            if visited.Contains(s.name) then
                                currentState
                            else
                                visited.Add(s.name) |> ignore
                                match s with
                                | Working -> walkDependencies visited currentState (Array.concat([deps; s.links]))
                                | _ -> walkDependencies visited ServiceLevel.Degraded (Array.concat([deps; s.links]))



let walkService service =
    let visited = Set<string>([])
    match service with
    | Working -> walkDependencies visited Working service.links
    | Unavailable -> Unavailable

let determineServiceUptime count service =
    [1 .. count]
    |> List.map (fun _ -> walkService service)
    |> List.fold (fun (successes, failures, degradations) result ->
        match result with
        | ServiceLevel.Unavailable -> successes, failures + 1, degradations
        | ServiceLevel.Working -> successes + 1, failures, degradations
        | ServiceLevel.Degraded -> successes, failures, degradations + 1) (0, 0, 0)
