module Translations

open Model

let rec copy (filter: Link -> bool) (start) =
    // walk the relationships and copy them if the filter returns true
    { start with links = start.links
                        |> Array.filter filter
                        |> Array.map (fun (link) ->
                            { link with relationship =
                                match link.relationship with
                                | Requires(s) -> Requires(copy filter s)
                                | BenefitsFrom(s) -> BenefitsFrom(copy filter s)
                                | ComposedOf(s) -> ComposedOf(copy filter s)
                                | ResponsibilityOf(s) -> ResponsibilityOf(copy filter s)} ) }

let extractComponent link =
    match link.relationship with
    | Requires(s) -> s
    | BenefitsFrom(s) -> s
    | ComposedOf(s) -> s
    | ResponsibilityOf(s) -> s

/// Translates the given component and relationships into a
/// different representation
let translate (mapComponent: Component -> 'a) (mapLink: Link -> 'a -> 'b) (attach: 'a -> 'b array -> 'a) start =
    let mutable cache = Map<Component, 'a>([])

    let memo f input =
        if cache.ContainsKey(input) then
            cache.[input]
        else
            let result = f input
            cache <- (Map.add input result cache)
            result


    let rec dotranslate (mapComponent: Component -> 'a) (mapLink: Link -> 'a -> 'b) (attach: 'a -> 'b array -> 'a) start =
        let newState = mapComponent start

        // walk the relationships and translate them
        let newLinks = start.links
                        |> Array.map (fun (link) ->
                            let newComponent = extractComponent link
                                            |> memo (dotranslate mapComponent mapLink attach)
                            mapLink link newComponent)

        attach newState newLinks

    memo (dotranslate mapComponent mapLink attach) start

