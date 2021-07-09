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

    [<TestMethod>]
    member this.TestSegmentsIntersectParallel1 () =
        let seg1 = (Coord (0, 0), Coord (0, 2))
        let seg2 = (Coord (2, 0), Coord (2, 2))
        let expected = Parallel
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)
 
    [<TestMethod>]
    member this.TestSegmentsIntersectParallel2 () =
        let seg1 = (Coord (4, 2), Coord (8, 4))
        let seg2 = (Coord (2, 0), Coord (6, 2))
        let expected = Parallel
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectParallel3 () =
        let seg1 = (Coord (8, 4), Coord (4, 2))
        let seg2 = (Coord (2, 0), Coord (6, 2))
        let expected = Parallel
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap1 () =
        let seg1 = (Coord (0, 0), Coord (4, 4))
        let seg2 = (Coord (1, 1), Coord (2, 2))
        let expected = Overlap (0.25, 0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap2 () =
        let seg1 = (Coord (2, 2), Coord (6, 4))
        let seg2 = (Coord (4, 3), Coord (8, 5))
        let expected = Overlap (0.5, 1.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap3 () =
        let seg1 = (Coord (9, 6), Coord (1, 2))
        let seg2 = (Coord (3, 3), Coord (5, 4))
        let expected = Overlap (0.75, 0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap4 () =
        let seg1 = (Coord (6, 7), Coord (7, 8))
        let seg2 = (Coord (2, 3), Coord (1, 2))
        let expected = Overlap (-4.0, -5.0)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint1 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (2, 0), Coord (0, 2))
        let expected = Point (0.5, 0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint2 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (1, 0), Coord (0, 1))
        let expected = Point (0.25, 0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)    

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint3 () =
        let seg1 = (Coord (0, 2), Coord (3, 7))
        let seg2 = (Coord (2, 0), Coord (3, 7))
        let expected = Point (1.0, 1.0)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)      

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint4 () =
        let seg1 = (Coord (3, 2), Coord (3, 7))
        let seg2 = (Coord (3, 7), Coord (-1, 2))
        let expected = Point (1.0, 0.0)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2) 

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint5 () =
        let seg1 = (Coord (4, 4), Coord (0, 0))
        let seg2 = (Coord (1, 7), Coord (5, -1))
        let expected = Point (0.25, 0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)      

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint6 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (0, 2), Coord (2, 0))
        let expected = Point (0.5, 0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint7 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (0, 2), Coord (-2, 4))
        let expected = Point (0.5, -0.5)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint8 () =
        let seg1 = (Coord (2, 2), Coord (3, 3))
        let seg2 = (Coord (-2, 2), Coord (-3, 3))
        let expected = Point (-2.0, -2.0)
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)


