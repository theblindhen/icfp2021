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
    member this.TestTranslateRandomCoord () =
        Assert.AreEqual(2, (Random 0).Next(4))
        let fig verticies = {
            Edges = Array.ofList []
            Vertices = verticies
        }
        let expectedSolutions =
            [
                fig (Array.ofList [Coord(10,10); Coord(10, 20); Coord(21, 20); Coord(20, 10)]);
                fig (Array.ofList [Coord(10,10); Coord(10, 20); Coord(20, 21); Coord(20, 10)]);
                fig (Array.ofList [Coord(10,10); Coord(10, 20); Coord(19, 20); Coord(20, 10)]);
                fig (Array.ofList [Coord(10,10); Coord(10, 20); Coord(20, 19); Coord(20, 10)]);
            ]
        Assert.AreEqual(expectedSolutions,
            Neighbors.translateRandomCoord (Random 0) (fig (Array.ofList [Coord(10,10); Coord(10, 20); Coord(20, 20); Coord(20, 10)])))