module Model
open System

type ServiceType =
    | External
    | Internal

type ServiceResult =
    | Success of TimeSpan
    | Failure of TimeSpan

type ReliabilityProfile = {
    works: unit -> ServiceResult
}

let randomUptimeProfile uptime =
    let rand = new Random()
    {
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
and Component = {
            name: string
            links: relationships array
            serviceType: ServiceType
            reliabilityProfile: ReliabilityProfile
        }
type ServiceLevel =
    | Working
    | Unavailable
    | Degraded

let (| Working | Unavailable | ) (service: Component) =
    match service.reliabilityProfile.works() with
    | Success(_) -> Working
    | _ -> Unavailable

let mitigatedBy strategy (service: Component) =
    {service with reliabilityProfile = strategy(service.reliabilityProfile)}

let rec walkDependencies (visited: Set<string>) currentState (dependencies: relationships array) =

    if dependencies.Length = 0 then
        currentState
    else
        let head = dependencies.[0]
        let deps = dependencies.[1..dependencies.Length - 1]

        match head with
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
