module Reliability.Patterns

open Model
open System

let retrying retries targetProfile =
    {
        shorthand = sprintf "retrying %d times" retries
        works = fun () ->
                    let rec attempt r =
                        if retries = 0 then
                            Unavailable(TimeSpan.FromMilliseconds(1))
                        else
                            match targetProfile.works() with
                            | Working(_) -> Working(TimeSpan.FromMilliseconds(1))
                            | Degraded(_) -> Degraded(TimeSpan.FromMilliseconds(1))
                            | Unavailable(time) -> attempt (r - 1)

                    attempt retries
    }
