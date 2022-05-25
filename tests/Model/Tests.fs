module Tests

open System
open Xunit
open Model
open Reliability.Patterns

let ``a totally reliable service`` = 
    {
        name = "a totally reliable service"
        links = []
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

[<Fact>]
let ``Reliable services are always reliable`` () =
    let result = walkService ``a totally reliable service`` 

    Assert.StrictEqual(ServiceLevel.Working, result)

[<Theory>]
[<InlineData(0.90, 1000, 890, 910)>]
[<InlineData(0.50, 1000, 475, 525)>]
let ``Unreliable Services are accurately unreliable`` (uptime, iterations, minExpected, maxExpected) =
    let ``unreliable service`` = {
        name = "an ureliable service"
        links = []
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let successes, _, _ = determineServiceUptime iterations ``unreliable service``

    Assert.InRange(successes, minExpected, maxExpected)

[<Theory>]
[<InlineData(0.90, 1000, 880, 920)>]
[<InlineData(0.50, 1000, 480, 520)>]
let ``Unreliable dependencies make your architecture unreliable`` (uptime, iterations, minExpected, maxExpected) =
    let ``unreliable service`` = {
        name = "an ureliable service"
        links = []
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [Requires(``unreliable service``)]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

    let successes, _, _ = determineServiceUptime iterations ``unreliable service``

    Assert.InRange(successes, minExpected, maxExpected)


[<Theory>]
[<InlineData(0.90, 1000)>]
[<InlineData(0.50, 1000)>]
let ``Unreliable optional dependencies cause degradations not failures`` (uptime, iterations) =
    let ``unreliable service`` = {
        name = "an ureliable service"
        links = []
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [BenefitsFrom(``unreliable service``)]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

    let _, failures, _ = determineServiceUptime iterations ``my architecture``

    Assert.StrictEqual(0, failures)


[<Theory>]
[<InlineData(0.90, 1000, 950, 1000)>]
[<InlineData(0.50, 1000, 850, 1000)>]
let ``Retrying Unreliable Services improve reliability`` (uptime, iterations, minExpected, maxExpected) =
    let ``unreliable service`` = {
        name = "an ureliable service"
        links = []
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [Requires(``unreliable service`` |> mitigatedBy (retrying 3))]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

    let successes, _, _ = determineServiceUptime iterations ``my architecture``

    Assert.InRange(successes, minExpected, maxExpected)