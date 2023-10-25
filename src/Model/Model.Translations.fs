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
        |> Set.ofSeq
        |> Set.toSeq

    let links' =
        links
        |> Seq.map (fun link ->
            let on' = nodeTranslator link.on
            let from' = nodeTranslator link.from
            linkTranslator link on' from')

    starts', links'

let translate nodeTransformer linkTransformer start =

    let starts', links' = translateMulti nodeTransformer linkTransformer (Seq.singleton start)

    let start' = starts' |> Seq.head
    start', links'

