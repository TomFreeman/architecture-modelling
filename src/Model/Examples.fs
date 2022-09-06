module Examples
open Model
open Reliability.Patterns

// Helper method to generate complex architectures for testing and demonstrations
let generateComplexArchitecture levels =
    let rand = System.Random()
    let maxNodesAtLevel = 10

    let generateLevel possibleLinks =
        [0..maxNodesAtLevel]
        |> List.map (fun (i) ->
            let name = sprintf "node %d" i
            let serviceType = if rand.NextDouble() > 0.5 then External else Internal
            let reliabilityProfile = randomUptimeProfile (rand.NextDouble() * 0.5 + 0.5)
            let links =
                possibleLinks
                |> List.filter (fun (j) -> rand.Next(3) = 0)
                |> List.map (fun (link) ->
                    if rand.NextDouble() > 0.5 then Requires(link |> mitigatedBy (retrying 5)) else BenefitsFrom(link))
                |> List.toArray
            {name = name; links = links; serviceType = serviceType; reliabilityProfile = reliabilityProfile})

    [0..levels]
    |> List.fold (fun acc level ->
        let newNodes = generateLevel acc
        newNodes @ acc) []
    |> List.find (fun (node) -> node.links.Length > 0)
