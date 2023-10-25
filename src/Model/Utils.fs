module Utils
open System.Collections.Generic

let memoize f =
    let dict = Dictionary<_, _>()

    fun c ->
        let exist, value = dict.TryGetValue c

        match exist with
        | true -> value
        | _ ->
            let value = f c
            dict.Add(c, value)
            value
