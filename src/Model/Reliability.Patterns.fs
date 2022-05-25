module Reliability.Patterns

open Model
open System

let retrying retries targetProfile = 
    {
        works = fun () -> 
                    let rec attempt r =
                        if retries = 0 then 
                            Failure(TimeSpan.FromMilliseconds(1))
                        else
                            match targetProfile.works() with
                            | Success(_) -> Success(TimeSpan.FromMilliseconds(1))
                            | Failure(time) -> attempt (r - 1)
                    
                    attempt retries
    }