namespace UnitTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);

    [<TestMethod>]
    member this.TestFrom () =
        Assert.AreEqual("from monkey", Program.from "monkey")

    [<TestMethod>]
    member this.TestHillClimber () =
        Assert.AreEqual(2.0, Hillclimber.runHillClimber (fun d -> [d - 0.5; d + 0.5]) id 1 2.5)
        Assert.AreEqual(1.5, Hillclimber.runHillClimber (fun d -> [d - 0.5; d + 0.5]) id 2 2.5)
        Assert.AreEqual(1.0, Hillclimber.runHillClimber (fun d -> [d - 0.5; d + 0.5]) id 3 2.5)
        Assert.AreEqual(0.5, Hillclimber.runHillClimber (fun d -> [d - 0.5; d + 0.5]) id 4 2.5)
        Assert.AreEqual(0.0, Hillclimber.runHillClimber (fun d -> [d - 0.5; d + 0.5]) id 5 2.5)
        Assert.AreEqual(0.0, Hillclimber.runHillClimber (fun d -> [d - 0.5; d + 0.5]) id 10 2.5)