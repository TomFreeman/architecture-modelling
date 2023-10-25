namespace Library.Tests

open System
open FsUnit
open NUnit.Framework
open Model
open Reliability.Patterns

[<TestFixture>]
type Tests() =

    let ``a totally reliable service`` =
        {
            name = "a totally reliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

    [<Test>]
    member this.``Reliable services are always reliable`` () =
        let profile = fetchReliabilityProfile ``a totally reliable service``

        let result = profile.works()

        result |> should be (ofCase <@ Working @>)

    [<Test>]
    [<TestCase(0.90, 1000, 880, 920)>]
    [<TestCase(0.50, 1000, 475, 525)>]
    member this. ``Unreliable Services are accurately unreliable`` (uptime, iterations, minExpected, maxExpected) =
        let ``unreliable service`` = {
            name = "an unreliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile uptime
            metadata = None
        }

        let successes, _, _ = determineServiceUptime iterations ``unreliable service``

        successes |> should be (greaterThanOrEqualTo minExpected)
        successes |> should be (lessThanOrEqualTo maxExpected)

    [<Test>]
    [<TestCase(0.90, 1000, 875, 925)>]
    [<TestCase(0.50, 1000, 480, 520)>]
    member this. ``Unreliable dependencies make your architecture unreliable`` (uptime, iterations, minExpected, maxExpected) =
        let ``unreliable service`` = {
            name = "an unreliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile uptime
            metadata = None
        }

        let ``my architecture`` = {
            name = "my architecture"
            serviceType = InternalService
            reliabilityProfile = perfectUptime
            metadata = None
        }

        let successes, _, _ = determineServiceUptime iterations ``unreliable service``

        successes |> should be (greaterThanOrEqualTo minExpected)
        successes |> should be (lessThanOrEqualTo maxExpected)


    [<Test>]
    [<TestCase(0.90, 1000)>]
    [<TestCase(0.50, 1000)>]
    member this.``Unreliable optional dependencies cause degradations not failures`` (uptime, iterations) =
        let ``my architecture`` = {
            name = "my architecture"
            serviceType = InternalService
            reliabilityProfile = perfectUptime
            metadata = None
        }

        ``my architecture`` >!> {
            name = "an unreliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile uptime
            metadata = None
        }

        let _, failures, _ = determineServiceUptime iterations ``my architecture``

        failures |> should equal 0


    [<Test>]
    [<TestCase(0.90, 1000, 950, 1000)>]
    [<TestCase(0.50, 1000, 850, 1000)>]
    member this.``Retrying Unreliable Services improve reliability`` (uptime, iterations, minExpected, maxExpected) =
        let ``unreliable service`` = {
            name = "an unreliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile uptime
            metadata = None
        }

        let ``my architecture`` = {
            name = "my architecture"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        ``my architecture`` |> dependsOn (``unreliable service`` |> mitigatedBy (retrying 3))


        let successes, _, _ = determineServiceUptime iterations ``my architecture``

        successes |> should be (greaterThanOrEqualTo minExpected)
        successes |> should be (lessThanOrEqualTo maxExpected)

    // [<Test>]
    // let ``Can generate large random architectures`` () =
    //     let target = Examples.generateComplexArchitecture 3

    //     Assert.NotNull(target)
    //     let successes, failures, degradations = determineServiceUptime 10 target.[0]
    //     Assert.True (successes >= 0, sprintf "Expected at least one success %d successes, %d failures, %d degradations" successes failures degradations)

    // type simpleBranch = {
    //     branch: simpleTree
    //     }
    // and simpleTree = {
    //     name: string
    //     branches: simpleBranch array
    // }

    // [<Test>]
    // let ``Can translate an architecture into something simpler`` () =
    //     let startingPoint = {
    //         name = "my architecture"
    //         links = [|Requires({
    //             name = "a totally reliable service";
    //             links = [||]; serviceType = InternalService; reliabilityProfile = randomUptimeProfile 1.0
    //             metadata = None
    //         })|] |> noMetadata
    //         serviceType = InternalService
    //         reliabilityProfile = randomUptimeProfile 1.0
    //         metadata = None
    //     }

    //     let simpleLeafFromComponent (c: Component) =
    //         { name = c.name; branches = [||] }

    //     let simpleBranchFromLink (link: Link) (leaf: simpleTree) =
    //         { branch = leaf }

    //     let growTree leaf branches =
    //         { leaf with branches = Array.concat([leaf.branches; branches]) }

    //     let output = Translations.translate simpleLeafFromComponent simpleBranchFromLink growTree startingPoint

    //     Assert.NotNull(output)
    //     Assert.StrictEqual("my architecture", output.name)
    //     Assert.Equal(1, output.branches.Length)
    //     Assert.Equal("a totally reliable service", output.branches.[0].branch.name)

    // [<Test>]
    // let ``Can translate an architecture into something simpler starting from multiple entry points`` () =
    //     let startingPoint1 = {
    //         name = "Entry point 1"
    //         links = [|Requires({
    //             name = "a totally reliable service";
    //             links = [||]; serviceType = InternalService; reliabilityProfile = randomUptimeProfile 1.0
    //             metadata = None
    //         })|] |> noMetadata
    //         serviceType = InternalService
    //         reliabilityProfile = randomUptimeProfile 1.0
    //         metadata = None
    //     }

    //     let startingPoint2 = {
    //         name = "Entry point 2"
    //         links = [|Requires({
    //             name = "a totally reliable service";
    //             links = [||]; serviceType = InternalService; reliabilityProfile = randomUptimeProfile 1.0
    //             metadata = None
    //         })|] |> noMetadata
    //         serviceType = InternalService
    //         reliabilityProfile = randomUptimeProfile 1.0
    //         metadata = None
    //     }

    //     let simpleLeafFromComponent (c: Component) =
    //         { name = c.name; branches = [||] }

    //     let simpleBranchFromLink (link: Link) (leaf: simpleTree) =
    //         { branch = leaf }

    //     let growTree leaf branches =
    //         { leaf with branches = Array.concat([leaf.branches; branches]) }

    //     let output = Translations.translateMulti simpleLeafFromComponent simpleBranchFromLink growTree [|startingPoint1; startingPoint2|]

    //     Assert.NotNull(output)
    //     Assert.Equal(output.Length, 2)
    //     Assert.StrictEqual("Entry point 1", output.[0].name)
    //     Assert.StrictEqual("Entry point 2", output.[1].name)

    [<Test>]
    member this.``Equality Comparison works`` () =
        let a = {
            name = "a totally reliable service";
            serviceType = InternalService;
            reliabilityProfile = randomUptimeProfile 1.0;
            metadata = None
        }

        let b = {
            name = "a totally reliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        let c = {
            name = "a totally different service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        a |> should equal b
        a |> should not' (equal c)

    [<Test>]
    member this.``Maps that contain components identify that fact``() =
        let a = {
            name = "a totally reliable service";
            serviceType = InternalService;
            reliabilityProfile = randomUptimeProfile 1.0;
            metadata = None
        }

        let b = {
            name = "a totally reliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        let c = {
            name = "a totally different service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        let targetMap = Map<_,_>([a, 1])
        Assert.True(targetMap.ContainsKey(a))
        Assert.True(targetMap.ContainsKey(b))
        Assert.False(targetMap.ContainsKey(c))

    // [<Test>]
    // let ``Stores only unique components in the translated cache`` () =
    //     let dependency = {
    //             name = "a unique dependency";
    //             serviceType = InternalService;
    //             reliabilityProfile = randomUptimeProfile 1.0
    //             metadata = None
    //         }

    //     let startingPoint = {
    //         name = "a component that requires a unique dependency";
    //         serviceType = InternalService
    //         reliabilityProfile = randomUptimeProfile 1.0
    //         metadata = None
    //     }

    //     startingPoint |> dependsOn dependency
    //     startingPoint |> dependsOn dependency

    //     let mutable invocations = 0
    //     let simpleLeafFromComponent (c: Component) =
    //         invocations <- invocations + 1
    //         { name = c.name; branches = [||] }

    //     let simpleBranchFromLink (link: Link) (leaf: simpleTree) =
    //         { branch = leaf }

    //     let growTree leaf branches =
    //         { leaf with branches = Array.concat([leaf.branches; branches]) }

    //     let output = Translations.translate simpleLeafFromComponent simpleBranchFromLink growTree startingPoint

    //     Assert.Equal(2, invocations)

    // [<Test>]
    // let ``Translate multi translates each node once`` () =
    //     let dependency = {
    //             name = "a unique dependency";
    //             links = [||];
    //             serviceType = InternalService;
    //             reliabilityProfile = randomUptimeProfile 1.0
    //             metadata = None
    //         }

    //     let startingPoint1 = {
    //         name = "a component that requires a unique dependency";
    //         links = [|Requires(dependency); Requires(dependency)|] |> noMetadata
    //         serviceType = InternalService
    //         reliabilityProfile = randomUptimeProfile 1.0
    //         metadata = None
    //     }

    //     let startingPoint2 = {
    //         name = "another component that requires a unique dependency";
    //         links = [|Requires(dependency); Requires(dependency)|] |> noMetadata
    //         serviceType = InternalService
    //         reliabilityProfile = randomUptimeProfile 1.0
    //         metadata = None
    //     }

    //     let mutable invocations = 0
    //     let simpleLeafFromComponent (c: Component) =
    //         invocations <- invocations + 1
    //         { name = c.name; branches = [||] }

    //     let simpleBranchFromLink (link: Link) (leaf: simpleTree) =
    //         { branch = leaf }

    //     let growTree leaf branches =
    //         { leaf with branches = Array.concat([leaf.branches; branches]) }

    //     Translations.translateMulti simpleLeafFromComponent simpleBranchFromLink growTree [|startingPoint1; startingPoint2|]
    //     |> ignore

    //     Assert.Equal(3, invocations)
