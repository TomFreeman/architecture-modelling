module Translations

open Model

let rec copy (filter: relationships -> bool) (start: Component) =
    // walk the relationships and copy them if the filter returns true
    { start with links = start.links
                        |> Array.filter filter
                        |> Array.map (fun (link) ->
                            match link with
                            | Requires(s) -> Requires(copy filter s)
                            | BenefitsFrom(s) -> BenefitsFrom(copy filter s)
                            | ComposedOf(s) -> ComposedOf(copy filter s))}

let extractComponent relationship =
    match relationship with
    | Requires(s) -> s
    | BenefitsFrom(s) -> s
    | ComposedOf(s) -> s

/// Translates the given component and relationships into a
/// different representation
let rec translate (mapComponent: Component -> 'a) (mapLink: relationships -> 'a -> 'b) (attach: 'a -> 'b array -> 'a) start =
    let newState = mapComponent start

    // walk the relationships and translate them
    let newLinks = start.links
                    |> Array.map (fun (link) ->
                        let newComponent = extractComponent link
                                           |> translate mapComponent mapLink attach
                        mapLink link newComponent)

    attach newState newLinks

