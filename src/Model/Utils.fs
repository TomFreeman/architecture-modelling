module Utils
open System.Collections.Generic

let memoize targetFunction =
    let dict = Dictionary<int, _>()

    fun input ->
        let hash = input.GetHashCode()
        let exist, value = dict.TryGetValue hash

        match exist with
        | true -> value
        | _ ->
            let value = targetFunction input
            dict.Add(hash, value)
            value
