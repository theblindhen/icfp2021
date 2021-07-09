namespace UnitTests

open System
open Model
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

    [<TestMethod>]
    member this.TestTranslateRandomCoord () =
        Assert.AreEqual(2, (Random 0).Next(4))
        let expectedSolutions =
            [
                { SolutionVertices  = Array.ofList [Coord(10,10); Coord(10, 20); Coord(21, 20); Coord(20, 10)] };
                { SolutionVertices  = Array.ofList [Coord(10,10); Coord(10, 20); Coord(20, 21); Coord(20, 10)] };
                { SolutionVertices  = Array.ofList [Coord(10,10); Coord(10, 20); Coord(19, 20); Coord(20, 10)] };
                { SolutionVertices  = Array.ofList [Coord(10,10); Coord(10, 20); Coord(20, 19); Coord(20, 10)] };
            ]
        Assert.AreEqual(expectedSolutions,
            Neighbors.translateRandomCoord (Random 0) ({
                SolutionVertices = Array.ofList [Coord(10,10); Coord(10, 20); Coord(20, 20); Coord(20, 10)]
            }))