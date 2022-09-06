module Tests

open System
open Xunit
open Model
open Reliability.Patterns

let ``a totally reliable service`` =
    {
        name = "a totally reliable service"
        links = [||]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

[<Fact>]
let ``Reliable services are always reliable`` () =
    let result = walkService ``a totally reliable service``

    Assert.StrictEqual(ServiceLevel.Working, result)

[<Theory>]
[<InlineData(0.90, 1000, 880, 920)>]
[<InlineData(0.50, 1000, 475, 525)>]
let ``Unreliable Services are accurately unreliable`` (uptime, iterations, minExpected, maxExpected) =
    let ``unreliable service`` = {
        name = "an ureliable service"
        links = [||]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let successes, _, _ = determineServiceUptime iterations ``unreliable service``

    Assert.InRange(successes, minExpected, maxExpected)

[<Theory>]
[<InlineData(0.90, 1000, 875, 925)>]
[<InlineData(0.50, 1000, 480, 520)>]
let ``Unreliable dependencies make your architecture unreliable`` (uptime, iterations, minExpected, maxExpected) =
    let ``unreliable service`` = {
        name = "an ureliable service"
        links = [||]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [|Requires(``unreliable service``)|]
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
        links = [||]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [|BenefitsFrom(``unreliable service``)|]
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
        links = [||]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile uptime
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [|Requires(``unreliable service`` |> mitigatedBy (retrying 3))|]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

    let successes, _, _ = determineServiceUptime iterations ``my architecture``

    Assert.InRange(successes, minExpected, maxExpected)

[<Fact>]
let ``Can generate large random architectures`` () =
    let target = Examples.generateComplexArchitecture 3

    Assert.NotNull(target)
    let successes, failures, degradations = determineServiceUptime 10 target
    Assert.True (successes >= 0, sprintf "Expected at least one success %d successes, %d failures, %d degradations" successes failures degradations)

type simpleBranch = {
    branch: simpleTree
    }
and simpleTree = {
    name: string
    branches: simpleBranch array
}

[<Fact>]
let ``Can translate an architecture into something simpler`` () =
    let startingPoint = {
        name = "my architecture"
        links = [|Requires({
            name = "a totally reliable service"; links = [||]; serviceType = Internal; reliabilityProfile = randomUptimeProfile 1.0})|]
        serviceType = Internal
        reliabilityProfile = randomUptimeProfile 1.0
    }

    let simpleLeafFromComponent (component: Component) =
        { name = component.name; branches = [||] }

    let simpleBranchFromLink (link: relationships) (leaf: simpleTree) =
        { branch = leaf }

    let growTree leaf branches =
        { leaf with branches = Array.concat([leaf.branches; branches]) }

    let output = Translations.translate simpleLeafFromComponent simpleBranchFromLink growTree startingPoint

    Assert.NotNull(output)
    Assert.StrictEqual("my architecture", output.name)
    Assert.Equal(1, output.branches.Length)
    Assert.Equal("a totally reliable service", output.branches.[0].branch.name)
