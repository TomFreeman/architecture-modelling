module Translations

open Model

let fetchAllLinks start =
    let components = fetchComprisedOf start
    let requirements = fetchDependencies start
    let enhancements = fetchEnhancedBy start

    requirements
    |> Seq.append enhancements
    |> Seq.append components
    |> Set.ofSeq
    |> Set.toSeq
