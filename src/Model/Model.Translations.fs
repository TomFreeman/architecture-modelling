module Translations

open Model
open Utils

let fetchAllLinks (model: Model) start =
    let components = model.fetchComprisedOf start
    let requirements = model.fetchDependencies start
    let enhancements = model.fetchEnhancedBy start

    requirements
    |> Seq.append enhancements
    |> Seq.append components
    |> Set.ofSeq
    |> Set.toSeq

let traverse model visitNode visitLink starts =
    let nodeTranslator = memoize visitNode
    let linkTranslator = memoize visitLink

    let rec visit node =

        // We do return the starting nodes as a convenience, for folks who want their starting node(s) to be
        // the new root node(s)
        let parent = nodeTranslator node

        (fetchAllLinks model node)
        |> Seq.map (fun n ->
            linkTranslator n parent (visit n.on))
        |> Seq.iter ignore // We don't know what structure you want to hold your new stuff in, so you need to keep track yourself

        parent

    starts
    |> Seq.map visit

let rec debug (model:Model) component =

    printf "%s (%A)" component.name component.serviceType

    let dependencies = model.fetchDependencies component
    if dependencies |> Seq.isEmpty |> not then
        printfn "Requires:"
        dependencies
        |> Seq.iter (fun (link) -> (debug model link.on))

    let enhancements = model.fetchEnhancedBy component
    if enhancements |> Seq.isEmpty |> not then
        printfn "Enhanced by:"
        enhancements
        |> Seq.iter (fun (link) ->  (debug model link.on))

    let comprisedOf = model.fetchComprisedOf component
    if comprisedOf |> Seq.isEmpty |> not then
        printfn "Comprised of:"
        comprisedOf
        |> Seq.iter (fun (link) -> (debug model link.on))


