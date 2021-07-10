namespace UnitTests

open System
open Model
open Geometry
open Penalty
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SegmentOutsideHoleTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))
    let testHole1 = [
        seg (1, 1) (3, 1);
        seg (3, 1) (3, 3);
        seg (3, 3) (1, 3);
        seg (1, 3) (1, 1);
    ]
    let testHole2 = [
        seg (2, 0) (1, 1);
        seg (1, 1) (2, 2);
        seg (2, 2) (3, 1);
        seg (3, 1) (2, 0);
    ]
    let testHole2 = [
        seg (2, 0) (1, 1);
        seg (1, 1) (2, 2);
        seg (2, 2) (3, 1);
        seg (3, 1) (2, 0);
    ]
    let testHole3 = [
        seg (-2,6) (2,0)
        seg (2,0) (4,3)
        seg (4,3) (2,6)
        seg (2,6) (4,9)
        seg (4,9) (6,6)
        seg (6,6) (8,9)
        seg (8,9) (4,15)
        seg (4,15) (-2,6)
    ]

    [<TestMethod>]
    member this.TestIsCoordInsideHole1 () = 
        Assert.AreEqual(true, isCoordInsideHole testHole1 (Coord(1,1)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole2 () = 
        Assert.AreEqual(true, isCoordInsideHole testHole1 (Coord(2,2)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole3 () = 
        Assert.AreEqual(true, isCoordInsideHole testHole1 (Coord(3,3)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole4 () = 
        Assert.AreEqual(false, isCoordInsideHole testHole1 (Coord(-1,3)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole5 () = 
        Assert.AreEqual(false, isCoordInsideHole testHole1 (Coord(4,4)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole6 () = 
        Assert.AreEqual(true, isCoordInsideHole testHole2 (Coord(1,1)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole7 () = 
        Assert.AreEqual(true, isCoordInsideHole testHole2 (Coord(2,2)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole8 () = 
        Assert.AreEqual(true, isCoordInsideHole testHole2 (Coord(2,0)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole9 () = 
        Assert.AreEqual(false, isCoordInsideHole testHole2 (Coord(3,0)))

    [<TestMethod>]
    member this.TestIsCoordInsideHole10 () = 
        Assert.AreEqual(false, isCoordInsideHole testHole2 (Coord(3,3)))

    [<TestMethod>]
    member this.TestSegmentOutsideHole1 () = 
        let segment = seg (2,2) (3,3) 
        Assert.AreEqual(0.0, segmentOutsideHole testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentOutsideHole2 () = 
        let segment = seg (2,2) (4,4) 
        Assert.AreEqual(0.5, segmentOutsideHole testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentOutsideHole3 () = 
        let segment = seg (0,0) (4,4) 
        Assert.AreEqual(0.5, segmentOutsideHole testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentOutsideHole4 () = 
        let segment = seg (0,2) (8,2) 
        Assert.AreEqual(0.25, segmentOutsideHole testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentOutsideHole5 () = 
        let segment = seg (2,0) (1,1) 
        Assert.AreEqual(0.0, segmentOutsideHole testHole2 segment)

    [<TestMethod>]
    member this.TestSegmentOutsideHole6 () = 
        let segment = seg (1,1) (2,0)
        Assert.AreEqual(0.0, segmentOutsideHole testHole2 segment)

    [<TestMethod>]
    member this.TestSegmentOutsideHole7 () = 
        let segment = seg (0,-3) (10,12)
        Assert.AreEqual(0.6, segmentOutsideHole testHole3 segment)

    [<TestMethod>]
    member this.TestSortSegments () =
        let segments =
            [
               (Coord (2, 0), Coord (4, 2));
               (Coord (2, 0), Coord (0, 2));
               (Coord (4, 2), Coord (2, 4));
               (Coord (2, 4), Coord (0, 2));
            ]
        let expectedSolutions =
            [
               (Coord (2, 0), Coord (0, 2));
               (Coord (2, 0), Coord (4, 2));
               (Coord (0, 2), Coord (2, 4));
               (Coord (4, 2), Coord (2, 4));
            ]
        Assert.AreEqual(expectedSolutions, sortSegments segments)