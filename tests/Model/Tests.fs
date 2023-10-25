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
    branches: simpleBranch array
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

    [<SetUp>]
    member this.Setup() =
        Dependencies.Clear()
        EnhancedBy.Clear()
        ComprisedOf.Clear()
        ResponsibleFor.Clear()

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

    [<Test>]
    member this.``Can generate large random architectures`` () =
        let target = Examples.generateComplexArchitecture 3

        Assert.NotNull(target)
        let successes, failures, degradations = determineServiceUptime 10 target.[0]
        Assert.True (successes >= 0, sprintf "Expected at least one success %d successes, %d failures, %d degradations" successes failures degradations)

    [<Test>]
    member this. ``Can translate an architecture into something simpler`` () =
        let startingPoint = {
            name = "my architecture"
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint |> dependsOn ``a totally reliable service``

        let simpleLeafFromComponent (c: Component) =
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            trunk.branches |> Array.append [|branch|]

        let output, branches = Translations.translate simpleLeafFromComponent simpleBranchFromLink startingPoint

        output |> should not' (be null)

        output.name |> should equal "my architecture"

        branches |> should not' (be null)
        branches |> Seq.length |> should equal 1
        let first = branches |> Seq.head |> Array.head
        first.branch.name |> should equal "a totally reliable service"

    [<Test>]
    member this. ``Can translate an architecture into something simpler starting from multiple entry points`` () =
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

        startingPoint1 |> dependsOn ``reliable``
        startingPoint2 |> dependsOn ``reliable``

        let simpleLeafFromComponent (c: Component) =
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            trunk.branches |> Array.append [|branch|]
        let output, branches = Translations.translateMulti simpleLeafFromComponent simpleBranchFromLink [|startingPoint1; startingPoint2|]

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
        Assert.True(targetMap.ContainsKey(a))
        Assert.True(targetMap.ContainsKey(b))
        Assert.False(targetMap.ContainsKey(c))

    [<Test>]
    member this. ``Stores only unique components in the translated cache`` () =
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

        startingPoint |> dependsOn dependency
        startingPoint |> comprisedOf dependency

        let mutable invocations = 0
        let simpleLeafFromComponent (c: Component) =
            invocations <- invocations + 1
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            trunk.branches |> Array.append [|branch|]

        let output, branches = Translations.translate simpleLeafFromComponent simpleBranchFromLink startingPoint

        // Laziness and using mutables do not mix well, force the issue
        let bs = branches |> Seq.toArray
        invocations |> should equal 2

    [<Test>]
    member this. ``Translate multi translates each node once`` () =
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

        startingPoint1 |> dependsOn dependency
        startingPoint1 |> comprisedOf dependency

        let startingPoint2 = {
            name = "another component that requires a unique dependency";
            serviceType = InternalService
            reliabilityProfile = randomUptimeProfile 1.0
            metadata = None
        }

        startingPoint2 |> dependsOn dependency
        startingPoint2 |> comprisedOf dependency

        let mutable invocations = 0
        let simpleLeafFromComponent (c: Component) =
            invocations <- invocations + 1
            { name = c.name; branches = [||] }

        let simpleBranchFromLink (link: Link) branch trunk  =
            let branch = {
                branch = branch
            }
            trunk.branches |> Array.append [|branch|]

        let outputs, branches = Translations.translateMulti simpleLeafFromComponent simpleBranchFromLink [|startingPoint1; startingPoint2|]

        // Laziness and using mutables do not mix well, force the issue
        let bs = branches |> Seq.toArray
        invocations |> should equal 3
