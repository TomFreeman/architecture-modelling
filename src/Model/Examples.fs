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
            let serviceType = if rand.NextDouble() > 0.5 then ExternalService else InternalService
            let reliabilityProfile = randomUptimeProfile (rand.NextDouble() * 0.5 + 0.5)

            {name = name; serviceType = serviceType; reliabilityProfile = reliabilityProfile; metadata = None})

    let components =
        [0..levels]
        |> List.map generateLevel

    // Randomly link nodes from each level to nodes from the next level
    for i in [0..levels - 1] do
        let currentLevel = components.[i]
        let nextLevel = components.[i + 1]

        for node in currentLevel do
            let links = rand.Next(0, 3)
            for i in [0..links] do
                let target = nextLevel |> List.item (rand.Next(0, nextLevel.Length))

                match rand.Next(1, 3) with
                | 1 -> node >=> target
                | 2 -> node >-> target
                | _ -> node >!> target

    List.concat components
