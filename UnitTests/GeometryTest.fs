namespace UnitTests

open System
open Model
open Geometry
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GeometryTestClass () =

    [<TestMethod>]
    member this.TestSortSegment1 () =
        let segment1 = (Coord (2, 0), Coord (4, 2));
        let expected1 = (Coord (2, 0), Coord (4, 2));
        Assert.AreEqual(expected1, sortSegment segment1)

    [<TestMethod>]
    member this.TestSortSegment2 () =
        let segment1 = (Coord (2, 2), Coord (3, 1));
        let expected1 = (Coord (3, 1), Coord (2, 2));
        Assert.AreEqual(expected1, sortSegment segment1)

    [<TestMethod>]
    member this.TestSortSegments () =
        let segments =
            [
               (Coord (2, 0), Coord (4, 2));
               (Coord (2, 0), Coord (0, 2));
               (Coord (4, 2), Coord (2, 4));
               (Coord (2, 4), Coord (0, 2))
            ]
        let expectedSolutions =
            [
               (Coord (2, 0), Coord (0, 2));
               (Coord (2, 0), Coord (4, 2));
               (Coord (0, 2), Coord (2, 4))
               (Coord (4, 2), Coord (2, 4));
            ]
        Assert.AreEqual(expectedSolutions, sortSegments segments)