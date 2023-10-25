module Translations

open Model
open Utils

let fetchAllLinks start =
    let components = fetchComprisedOf start
    let requirements = fetchDependencies start
    let enhancements = fetchEnhancedBy start

    requirements
    |> Seq.append enhancements
    |> Seq.append components
    |> Set.ofSeq
    |> Set.toSeq

let translateMulti nodeTransformer linkTransformer starts =
    let nodeTranslator = memoize nodeTransformer
    let linkTranslator = memoize linkTransformer

    let starts' =
        starts
        |> Seq.map nodeTranslator

    let links =
        starts
        |> Seq.map fetchAllLinks
        |> Seq.concat
        |> Seq.toArray

    let links' =
        links
        |> Seq.map (fun link ->
            let on' = nodeTranslator link.on
            let from' = nodeTranslator link.from
            linkTranslator link on' from')
        |> Seq.toArray

    starts', links'

let translate nodeTransformer linkTransformer start =

    let starts', links' = translateMulti nodeTransformer linkTransformer (Seq.singleton start)

    let start' = starts' |> Seq.head
    start', links'

let rec debug component =

    printf "%s (%A)" component.name component.serviceType

    let dependencies = fetchDependencies component
    if dependencies |> Seq.isEmpty |> not then
        printfn "Requires:"
        dependencies
        |> Seq.iter (fun (link) -> (debug link.on))

    let enhancements = fetchEnhancedBy component
    if enhancements |> Seq.isEmpty |> not then
        printfn "Enhanced by:"
        enhancements
        |> Seq.iter (fun (link) ->  (debug link.on))

    let comprisedOf = fetchComprisedOf component
    if comprisedOf |> Seq.isEmpty |> not then
        printfn "Comprised of:"
        comprisedOf
        |> Seq.iter (fun (link) -> (debug link.on))


