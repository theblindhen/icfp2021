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
    // Inspired by Problem 4
    let wedgeHole = 
        [
            seg (70,90) (50,95)
            seg (50,95) (30,90)
            seg (30,90) (30,0)
            seg (30, 0) (70,0)
            seg (70,0) (70,90)
        ]
    let sharpWedgeHole = 
        [
            seg (0, 20) (2, 10)
            seg (2, 10) (4, 20)
            seg (4, 20) (0, 20)
        ]
    let invSharpWedgeHole = 
        [
            seg (-30, -100) (0, 20)
            seg (0, 20) (2, 10)
            seg (2, 10) (4, 20)
            seg (4, 20) (34, 100)
            seg (34, -100) (0, -30)
        ]
    // Inspired by Problem 13
    let largeDiamond = 
        [
            seg (20, 0) (40, 20)
            seg (40, 20) (20, 40)
            seg (20, 40) (0, 20)
            seg (0, 20) (20, 0)
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
    member this.TestIsCoordInsideHole11 () = 
        Assert.AreEqual(true, isCoordInsideHole wedgeHole (Coord(50,95)))



    [<TestMethod>]
    member this.TestSegmentStartsInside1 () = 
        let segment = seg (2,2) (3,3) 
        Assert.IsTrue(segmentStartsInside testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside2 () = 
        let segment = seg (2,2) (4,4) 
        Assert.IsTrue(segmentStartsInside testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside3 () = 
        let segment = seg (0,0) (4,4) 
        Assert.IsFalse(segmentStartsInside testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside4 () = 
        let segment = seg (0,2) (8,2) 
        Assert.IsFalse(segmentStartsInside testHole1 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside5 () = 
        let segment = seg (2,0) (1,1) 
        Assert.IsFalse(segmentStartsInside testHole2 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside6 () = 
        let segment = seg (1,1) (2,0)
        Assert.IsFalse(segmentStartsInside testHole2 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside7 () = 
        let segment = seg (0,-3) (4,3)
        Assert.IsFalse(segmentStartsInside testHole3 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside8 () = 
        let segment = seg (0,-3) (6,6)
        Assert.IsFalse(segmentStartsInside testHole3 segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside9 () = 
        let segment = seg (20,19) (20,5)
        Assert.IsTrue(segmentStartsInside largeDiamond segment)

    [<TestMethod>]
    member this.TestSegmentStartsInside10 () = 
        let segment = seg (20,5) (20,19)
        Assert.IsTrue(segmentStartsInside largeDiamond segment)




    [<TestMethod>]
    member this.TestSegmentOutsideHole1 () = 
        let segment = seg (2,2) (3,3) 
        Assert.AreEqual(0.0, segmentOutsideHole testHole1 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole2 () = 
        let segment = seg (2,2) (4,4) 
        Assert.AreEqual(0.5, segmentOutsideHole testHole1 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole3 () = 
        let segment = seg (0,0) (4,4) 
        Assert.AreEqual(0.5, segmentOutsideHole testHole1 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole4 () = 
        let segment = seg (0,2) (8,2) 
        Assert.AreEqual(0.75, segmentOutsideHole testHole1 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole5 () = 
        let segment = seg (2,0) (1,1) 
        Assert.AreEqual(0.0, segmentOutsideHole testHole2 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole6 () = 
        let segment = seg (1,1) (2,0)
        Assert.AreEqual(0.0, segmentOutsideHole testHole2 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole7 () = 
        let segment = seg (0,-3) (4,3)
        Assert.AreEqual(0.5, segmentOutsideHole testHole3 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole8 () = 
        let segment = seg (0,-3) (6,6)
        Assert.AreEqual(2.0/3.0, segmentOutsideHole testHole3 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole9 () = 
        let segment = seg (0,-3) (10,12)
        Assert.AreEqual(0.6, segmentOutsideHole testHole3 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole10 () = 
        let segment = seg (-1,-2) (9,13)
        Assert.AreEqual(0.6, segmentOutsideHole testHole3 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole11 () = 
        let segment = seg (-2,2) (8,17)
        Assert.AreEqual(0.4, segmentOutsideHole testHole3 segment, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole12 () = 
        let seg = seg (40,85) (50,95)
        Assert.AreEqual(0.0, segmentOutsideHole wedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole13 () = 
        let seg = seg (50,95) (60,85)
        Assert.AreEqual(0.0, segmentOutsideHole wedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole14 () = 
        let seg = seg (50,95) (50,100)
        Assert.AreEqual(1.0, segmentOutsideHole wedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole15 () = 
        let seg = seg (2, 10) (2, 0)
        Assert.AreEqual(1.0, segmentOutsideHole sharpWedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole16 () = 
        let seg = seg (2, 10) (10, 10)
        Assert.AreEqual(1.0, segmentOutsideHole sharpWedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole17 () = 
        let seg = seg (2, 10) (2, 0)
        Assert.AreEqual(0.0, segmentOutsideHole invSharpWedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole18 () = 
        let seg = seg (2, 10) (10, 10)
        Assert.AreEqual(0.0, segmentOutsideHole invSharpWedgeHole seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole19 () = 
        let seg = seg (20, 9) (20, 1)
        Assert.AreEqual(0.0, segmentOutsideHole largeDiamond seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole20 () = 
        let seg = seg (20, 9) (20, 1)
        Assert.AreEqual(0.0, segmentOutsideHole largeDiamond seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole21 () = 
        let seg = seg (1, 20) (9, 20)
        Assert.AreEqual(0.0, segmentOutsideHole largeDiamond seg, EPSILON)

    [<TestMethod>]
    member this.TestSegmentOutsideHole22 () = 
        let seg = seg (20, 6) (20, 37)
        Assert.AreEqual(0.0, segmentOutsideHole largeDiamond seg, EPSILON)

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

    [<TestMethod>]
    member this.TestPenaltyEdgeRatioOutside () = 
        let segment = seg (-2,2) (8,17)
        Assert.AreEqual(0.4, segmentOutsideHole testHole3 segment, EPSILON)
