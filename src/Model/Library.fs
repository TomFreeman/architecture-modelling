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
            links: relationships list
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

let rec walkDependencies currentState dependencies =
    match dependencies with
    | Requires(s) :: deps | ComposedOf(s) :: deps -> 
                        match s with
                        | Working -> walkDependencies currentState (deps @ s.links)
                        | Unavailable -> Unavailable
    | BenefitsFrom(s) :: deps -> 
                        match s with
                        | Working -> walkDependencies currentState (deps @ s.links)
                        | _ -> walkDependencies ServiceLevel.Degraded (deps @ s.links)
    | [] -> currentState


let walkService service =
    
    match service with
    | Working -> walkDependencies Working service.links
    | Unavailable -> Unavailable

let determineServiceUptime count service =
    [1 .. count]
    |> List.map (fun _ -> walkService service)
    |> List.fold (fun (successes, failures, degradations) result -> 
        match result with
        | ServiceLevel.Unavailable -> successes, failures + 1, degradations
        | ServiceLevel.Working -> successes + 1, failures, degradations
        | ServiceLevel.Degraded -> successes, failures, degradations + 1) (0, 0, 0)