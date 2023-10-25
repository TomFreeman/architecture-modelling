module Model
open System

type UnitType =
    | ExternalService
    | InternalService
    | Team
    | Individual

type ServiceLevel =
    | Working of TimeSpan
    | Unavailable of TimeSpan
    | Degraded of TimeSpan

[<CustomEquality; CustomComparison>]
type ReliabilityProfile = {
    shorthand: string
    works: unit -> ServiceLevel
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
    works = fun () -> Working (TimeSpan.FromMilliseconds(1))
}


let randomUptimeProfile uptime =
    let rand = new Random()
    {
        shorthand = sprintf "random %f" uptime
        works = fun () ->
                    if rand.NextDouble() > uptime then
                        Unavailable(TimeSpan.FromMilliseconds(1))
                    else
                        Working(TimeSpan.FromMilliseconds(1))
    }

type [<CustomEquality; CustomComparison>] Component = {
            name: string
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
                                                this.metadata = o.metadata
            | _ -> false

        override this.GetHashCode() =
            let hash =this.name.GetHashCode() +
                        this.serviceType.GetHashCode() +
                        this.reliabilityProfile.GetHashCode()

            if this.metadata = None then
                hash
            else
                hash + this.metadata.GetHashCode()

        interface System.IComparable with
            override this.CompareTo(other) =
                match other with
                | :? Component as o -> this.name.CompareTo(o.name)
                | _ -> 1


type [<CustomEquality; CustomComparison>] Link = {
    from: Component
    on: Component
    metadata: Map<string, string> option
} with
        override this.Equals(other) =
            match other with
            | :? Link as o -> this.on = o.on && this.from = o.from
            | _ -> false

        override this.GetHashCode() =
            this.on.GetHashCode() + this.from.GetHashCode()

        interface System.IComparable with
            override this.CompareTo(other) =
                match other with
                | :? Link as o -> this.on.name.CompareTo(o.on.name)
                | _ -> 1

let Dependencies = System.Collections.Generic.List<Link>()
let ComprisedOf = System.Collections.Generic.List<Link>()
let EnhancedBy = System.Collections.Generic.List<Link>()
let ResponsibleFor = System.Collections.Generic.List<Link>()

let dependsOn on from =
    Dependencies.Add { from = from; on = on; metadata = None }

let (>!>) on from =
    from |> dependsOn on

let comprisedOf on from =
    ComprisedOf.Add { from = from; on = on; metadata = None }

let (>=>) from on = from |> comprisedOf on

let enhancedBy on from =
    EnhancedBy.Add { from = from; on = on; metadata = None }

let (>->) from on = from |> enhancedBy on

let responsibleFor on from =
    ResponsibleFor.Add { from = from; on = on; metadata = None }

let (>~>) from on = from |> responsibleFor on

let plain link: Link =
    { link with metadata = None }

let noMetadata (relationships: Link seq) =
    relationships
    |> Seq.map (fun (relationship) -> plain relationship)
    |> Seq.toArray

let buildTeam teamname names metadata =
    let team = {
        name = teamname
        serviceType = Team
        reliabilityProfile = perfectUptime
        metadata = metadata
    }


    names
    |> Array.iter (fun (name) -> team >=> {name = name;
                        serviceType = Individual;
                        reliabilityProfile = perfectUptime;
                        metadata = None})

    team

let mitigatedBy strategy (service: Component) =
    {service with reliabilityProfile = strategy(service.reliabilityProfile)}

let fetchDependencies (service: Component) =
    Dependencies
    |> noMetadata
    |> Seq.filter (fun (link) -> link.from = service)

let fetchComprisedOf (service: Component) =
    ComprisedOf
    |> noMetadata
    |> Seq.filter (fun (link) -> link.from = service)

let fetchEnhancedBy (service: Component) =
    EnhancedBy
    |> noMetadata
    |> Seq.filter (fun (link) -> link.from = service)

let rec fetchReliabilityProfile service =
    let required = fetchDependencies service
    let enhancements = fetchEnhancedBy service
    let comprisedOf = fetchComprisedOf service

    let requiredProfiles =
        required
        |> Seq.append comprisedOf
        |> Seq.map (fun (link) -> link.on)
        |> Seq.map fetchReliabilityProfile
        |> Seq.toList

    let optionalProfiles =
        enhancements
        |> Seq.map (fun (link) -> link.on)
        |> Seq.map fetchReliabilityProfile
        |> Seq.toList

    let worstOf (serviceLevel1, serviceLevel2) =
        match serviceLevel1, serviceLevel2 with
        | Working(x), Working(y) -> if x > y then Working(x) else Working(y)
        | Working(x), Degraded(y) -> Degraded(y)
        | _, Unavailable(y) -> Unavailable(y)
        | Degraded(x), Working(y) -> Degraded(x)
        | Degraded(x), Degraded(y) -> if x > y then Degraded(x) else Degraded(y)
        | Unavailable(x), _ -> Unavailable(x)

    // memoize this as we'll end up visiting it twice as we check required and optional services
    let rec attempt serviceResult required profiles =
        match profiles with
        | [] -> serviceResult
        | profile :: rest ->
            match profile.works() with
            | Working(x) -> worstOf(attempt serviceResult required rest, Working(x))
            | Degraded(x) -> worstOf(attempt serviceResult required rest, Degraded(x))
            | Unavailable(x) -> if required then
                                    Unavailable(x)
                                else
                                    worstOf(attempt serviceResult required rest, Degraded(x))

    {
        shorthand = service.reliabilityProfile.shorthand
        works = fun () ->
            let serviceResult = service.reliabilityProfile.works()
            worstOf(attempt serviceResult true requiredProfiles, attempt serviceResult false optionalProfiles)
    }

let determineServiceUptime count service =
    let profile = fetchReliabilityProfile service

    [1 .. count]
    |> List.map (fun _ -> profile.works())
    |> List.fold (fun (successes, failures, degradations) result ->
        match result with
        | Unavailable(_) -> successes, failures + 1, degradations
        | Working(_) -> successes + 1, failures, degradations
        | Degraded(_) -> successes, failures, degradations + 1) (0, 0, 0)
