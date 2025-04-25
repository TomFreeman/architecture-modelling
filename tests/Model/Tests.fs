namespace Library.Tests

open System
open FsUnit
open NUnit.Framework
open Model
open Reliability.Patterns

type simpleBranch = {
    branch: simpleTree
    }
and simpleTree = {
    name: string
    mutable branches: simpleBranch array
}

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
        let arch = Model()
        let profile = arch.fetchReliabilityProfile ``a totally reliable service``

        let result = profile.works()

        result |> should be (ofCase <@ Working @>)

    [<Test>]
    [<TestCase(0.90, 1000, 880, 920)>]
    [<TestCase(0.50, 1000, 475, 525)>]
    member this. ``Unreliable Services are accurately unreliable`` (uptime, iterations, minExpected, maxExpected) =
        let arch = Model()
        let ``unreliable service`` = {
            name = "an unreliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile uptime
            metadata = None
        }

        let successes, _, _ = arch.determineServiceUptime iterations ``unreliable service``

        successes |> should be (greaterThanOrEqualTo minExpected)
        successes |> should be (lessThanOrEqualTo maxExpected)

    [<Test>]
    [<TestCase(0.90, 1000, 875, 925)>]
    [<TestCase(0.50, 1000, 480, 520)>]
    member this. ``Unreliable dependencies make your architecture unreliable`` (uptime, iterations, minExpected, maxExpected) =
        let arch = Model()
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

        let successes, _, _ = arch.determineServiceUptime iterations ``unreliable service``

        successes |> should be (greaterThanOrEqualTo minExpected)
        successes |> should be (lessThanOrEqualTo maxExpected)


    [<Test>]
    [<TestCase(0.90, 1000)>]
    [<TestCase(0.50, 1000)>]
    member this.``Unreliable optional dependencies cause degradations not failures`` (uptime, iterations) =
        let arch = Model()
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

        let _, failures, _ = arch.determineServiceUptime iterations ``my architecture``

        failures |> should equal 0


    [<Test>]
    [<TestCase(0.90, 1000, 950, 1000)>]
    [<TestCase(0.50, 1000, 850, 1000)>]
    member this.``Retrying Unreliable Services improve reliability`` (uptime, iterations, minExpected, maxExpected) =
        let arch = Model()
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

        ``my architecture`` |> arch.dependsOn (``unreliable service`` |> mitigatedBy (retrying 3))


        let successes, _, _ = arch.determineServiceUptime iterations ``my architecture``

        successes |> should be (greaterThanOrEqualTo minExpected)
        successes |> should be (lessThanOrEqualTo maxExpected)

    [<Test>]
    member this.``Can generate large random architectures`` () =
        let arch = Model()
        let target = Examples.generateComplexArchitecture 3

        Assert.NotNull(target)
        let successes, failures, degradations = arch.determineServiceUptime 10 target.[0]
        Assert.That (successes >= 0, sprintf "Expected at least one success %d successes, %d failures, %d degradations" successes failures degradations)

    [<Test>]
    member this. ``Can translate an architecture into something simpler`` () =
        let arch = Model()
        let startingPoint = {
            name = "my architecture"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint |> arch.dependsOn ``a totally reliable service``

        let simpleLeafFromComponent (c: Component) =
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) trunk branch  =
            let branch = {
                branch = branch
            }
            trunk.branches <- trunk.branches |> Array.append [|branch|]

        let output =
            Translations.traverse arch simpleLeafFromComponent simpleBranchFromLink [|startingPoint|]
            |> Seq.head // We only passed one root, so we should only have one in the translated model

        output |> should not' (be null)

        output.name |> should equal "my architecture"

        output.branches |> should not' (be null)
        output.branches |> Seq.length |> should equal 1
        let first = output.branches |> Seq.head
        first.branch.name |> should equal "a totally reliable service"

    [<Test>]
    member this. ``Can translate an architecture into something simpler starting from multiple entry points`` () =
        let arch = Model()
        let ``reliable`` = {
            name = "a totally reliable service"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        let startingPoint1 = {
            name = "Entry point 1"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        let startingPoint2 = {
            name = "Entry point 2"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint1 |> arch.dependsOn ``reliable``
        startingPoint2 |> arch.dependsOn ``reliable``

        let mutable branches = [||]

        let simpleLeafFromComponent (c: Component) =
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            trunk.branches <- trunk.branches |> Array.append [|branch|]
            branches <- branches |> Array.append [|branch|]

        let output = Translations.traverse arch simpleLeafFromComponent simpleBranchFromLink [|startingPoint1; startingPoint2|]
                                        |> Seq.toArray

        output |> should not' (be null)

        branches |> Seq.length |> should equal 2
        let outArray = output |> Seq.toArray
        outArray.[0].name |> should equal "Entry point 1"
        outArray.[1].name |> should equal "Entry point 2"

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
        Assert.That(targetMap.ContainsKey(a))
        Assert.That(targetMap.ContainsKey(b))
        targetMap.ContainsKey(c) |> should equal false

    [<Test>]
    member this. ``Stores only unique components in the translated cache`` () =
        let arch = Model()
        let dependency = {
                name = "a unique dependency";
                serviceType = InternalService;
                reliabilityProfile = randomUptimeProfile 1.0
                metadata = None
            }

        let startingPoint = {
            name = "a component that requires a unique dependency";
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint |> arch.dependsOn dependency
        startingPoint |> arch.comprisedOf dependency

        let mutable invocations = 0
        let mutable branches = [||]
        let simpleLeafFromComponent (c: Component) =
            invocations <- invocations + 1
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            branches <- branches |> Array.append [|branch|]


        Translations.traverse arch simpleLeafFromComponent simpleBranchFromLink [|startingPoint|]
        |> Seq.iter ignore

        // Laziness and using mutables do not mix well, force the issue
        let bs = branches |> Seq.toArray
        invocations |> should equal 2

    [<Test>]
    member this. ``Translate multi translates each node once`` () =
        let arch = Model()
        let dependency = {
                name = "a unique dependency";
                serviceType = InternalService;
                reliabilityProfile = randomUptimeProfile 1.0
                metadata = None
            }

        let startingPoint1 = {
            name = "a component that requires a unique dependency";
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint1 |> arch.dependsOn dependency
        startingPoint1 |> arch.comprisedOf dependency

        let startingPoint2 = {
            name = "another component that requires a unique dependency";
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint2 |> arch.dependsOn dependency
        startingPoint2 |> arch.comprisedOf dependency

        let mutable branches = [||]
        let mutable invocations = 0
        let simpleLeafFromComponent (c: Component) =
            invocations <- invocations + 1
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            branches <- branches |> Array.append [|branch|]

        let outputs = Translations.traverse arch simpleLeafFromComponent simpleBranchFromLink [|startingPoint1; startingPoint2|]
                                      |> Seq.toArray // Laziness and using mutables do not mix well, force the issue

        let bs = branches |> Seq.toArray
        invocations |> should equal 3
