module Tests

open System
open Xunit
open Model
open Reliability.Patterns

let ``a totally reliable service`` =
    {
        name = "a totally reliable service"
        links = [||]
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
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
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile uptime
        metadata = None
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
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile uptime
        metadata = None
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [|Requires(``unreliable service``)|] |> noMetadata
        serviceType = InternalService
        reliabilityProfile = perfectUptime
        metadata = None
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
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile uptime
        metadata = None
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [|plain(BenefitsFrom(``unreliable service``))|]
        serviceType = InternalService
        reliabilityProfile = perfectUptime
        metadata = None
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
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile uptime
        metadata = None
    }

    let ``my architecture`` = {
        name = "my architecture"
        links = [|plain(Requires(``unreliable service`` |> mitigatedBy (retrying 3)))|]
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
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
            name = "a totally reliable service";
            links = [||]; serviceType = InternalService; reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        })|] |> noMetadata
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
    }

    let simpleLeafFromComponent (c: Component) =
        { name = c.name; branches = [||] }

    let simpleBranchFromLink (link: Link) (leaf: simpleTree) =
        { branch = leaf }

    let growTree leaf branches =
        { leaf with branches = Array.concat([leaf.branches; branches]) }

    let output = Translations.translate simpleLeafFromComponent simpleBranchFromLink growTree startingPoint

    Assert.NotNull(output)
    Assert.StrictEqual("my architecture", output.name)
    Assert.Equal(1, output.branches.Length)
    Assert.Equal("a totally reliable service", output.branches.[0].branch.name)

[<Fact>]
let ``Equality Comparison works`` () =
    let a = {
        name = "a totally reliable service";
        links = [||];
        serviceType = InternalService;
        reliabilityProfile = randomUptimeProfile 1.0;
        metadata = None
    }

    let b = {
        name = "a totally reliable service"
        links = [||]
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
    }

    let c = {
        name = "a totally different service"
        links = [||]
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
    }

    Assert.StrictEqual(a, b)
    Assert.NotStrictEqual(a, c)

[<Fact>]
let ``Maps that contain components identify that fact``() =
    let a = {
        name = "a totally reliable service";
        links = [||];
        serviceType = InternalService;
        reliabilityProfile = randomUptimeProfile 1.0;
        metadata = None
    }

    let b = {
        name = "a totally reliable service"
        links = [||]
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
    }

    let c = {
        name = "a totally different service"
        links = [||]
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
    }

    let targetMap = Map<_,_>([a, 1])
    Assert.True(targetMap.ContainsKey(a))
    Assert.True(targetMap.ContainsKey(b))
    Assert.False(targetMap.ContainsKey(c))

[<Fact>]
let ``Stores only unique components in the translated cache`` () =
    let dependency = {
            name = "a unique dependency";
            links = [||];
            serviceType = InternalService;
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

    let startingPoint = {
        name = "a component that requires a unique dependency";
        links = [|Requires(dependency); Requires(dependency)|] |> noMetadata
        serviceType = InternalService
        reliabilityProfile = randomUptimeProfile 1.0
        metadata = None
    }

    let mutable invocations = 0
    let simpleLeafFromComponent (c: Component) =
        invocations <- invocations + 1
        { name = c.name; branches = [||] }

    let simpleBranchFromLink (link: Link) (leaf: simpleTree) =
        { branch = leaf }

    let growTree leaf branches =
        { leaf with branches = Array.concat([leaf.branches; branches]) }

    let output = Translations.translate simpleLeafFromComponent simpleBranchFromLink growTree startingPoint

    Assert.Equal(2, invocations)
