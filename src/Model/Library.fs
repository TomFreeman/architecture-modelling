module Model
open System
open System.Collections.Generic

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
    on: Component
    metadata: Map<string, string> option
} with
        override this.Equals(other) =
            match other with
            | :? Link as o -> this.on = o.on
            | _ -> false

        override this.GetHashCode() =
            this.on.GetHashCode()

        interface System.IComparable with
            override this.CompareTo(other) =
                match other with
                | :? Link as o -> this.on.name.CompareTo(o.on.name)
                | _ -> 1

let worstOf (serviceLevel1, serviceLevel2) =
    match serviceLevel1, serviceLevel2 with
    | Working(x), Working(y) -> if x > y then Working(x) else Working(y)
    | Working(x), Degraded(y) -> Degraded(y)
    | _, Unavailable(y) -> Unavailable(y)
    | Degraded(x), Working(y) -> Degraded(x)
    | Degraded(x), Degraded(y) -> if x > y then Degraded(x) else Degraded(y)
    | Unavailable(x), _ -> Unavailable(x)

let linksOrDefault (collection: Dictionary<'a, List<'b>>) item =
    if collection.ContainsKey(item) then
        collection[item]
    else
        List<'b>()

type Model() =
    class
        let Dependencies = Dictionary<Component, List<Link>>()
        let ComprisedOf = Dictionary<Component, List<Link>>()
        let EnhancedBy = Dictionary<Component, List<Link>>()
        let ResponsibleFor = Dictionary<Component, List<Link>>()

        member this.dependsOn on from =
            if Dependencies.ContainsKey(from) then
                Dependencies[from].Add({on = on; metadata = None })
            else
                Dependencies.Add(from, new List<Link>([{on = on; metadata = None}]))


        member this.comprisedOf on from =
            if ComprisedOf.ContainsKey(from) then
                ComprisedOf[from].Add({on = on; metadata = None })
            else
                ComprisedOf.Add(from, new List<Link>([{on = on; metadata = None}]))


        member this.enhancedBy on from =
            if EnhancedBy.ContainsKey(from) then
                EnhancedBy[from].Add({on = on; metadata = None })
            else
                EnhancedBy.Add(from, new List<Link>([{on = on; metadata = None}]))

        member this.responsibleFor on from =
            if ResponsibleFor.ContainsKey(from) then
                ResponsibleFor[from].Add({on = on; metadata = None })
            else
                ResponsibleFor.Add(from, new List<Link>([{on = on; metadata = None}]))


        member this.fetchDependencies service =
            linksOrDefault Dependencies service

        member this.fetchComprisedOf service =
            linksOrDefault ComprisedOf service

        member this.fetchEnhancedBy service =
            linksOrDefault EnhancedBy service

        member this.fetchResponsibleFor service =
            linksOrDefault ResponsibleFor service

        member this.fetchReliabilityProfile service =
            let required = this.fetchDependencies service
            let enhancements = this.fetchEnhancedBy service
            let comprisedOf = this.fetchComprisedOf service

            let requiredProfiles =
                required
                |> Seq.append comprisedOf
                |> Seq.map (fun (link) -> link.on)
                |> Seq.map this.fetchReliabilityProfile
                |> Seq.toList

            let optionalProfiles =
                enhancements
                |> Seq.map (fun (link) -> link.on)
                |> Seq.map this.fetchReliabilityProfile
                |> Seq.toList


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

            member this.determineServiceUptime count service =
                let profile = this.fetchReliabilityProfile service

                [1 .. count]
                |> List.map (fun _ -> profile.works())
                |> List.fold (fun (successes, failures, degradations) result ->
                    match result with
                    | Unavailable(_) -> successes, failures + 1, degradations
                    | Working(_) -> successes + 1, failures, degradations
                    | Degraded(_) -> successes, failures, degradations + 1) (0, 0, 0)
    end

let defaultArchitecture = Model()

let (>!>) on from =
    from |> defaultArchitecture.dependsOn on

let (>->) from on = from |> defaultArchitecture.enhancedBy on
let (>~>) from on = from |> defaultArchitecture.responsibleFor on
let (>=>) from on = from |> defaultArchitecture.comprisedOf on

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
