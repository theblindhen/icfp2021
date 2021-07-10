namespace UnitTests

open System
open Model
open Geometry
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GeometrySortTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))

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

[<TestClass>]
type GeometryIntersectTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))

    [<TestMethod>]
    member this.TestSegmentsIntersectParallel1 () =
        let seg1 = (Coord (0, 0), Coord (0, 2))
        let seg2 = (Coord (2, 0), Coord (2, 2))
        let expected = None
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)
 
    [<TestMethod>]
    member this.TestSegmentsIntersectParallel2 () =
        let seg1 = (Coord (4, 2), Coord (8, 4))
        let seg2 = (Coord (2, 0), Coord (6, 2))
        let expected = None
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectParallel3 () =
        let seg1 = (Coord (8, 4), Coord (4, 2))
        let seg2 = (Coord (2, 0), Coord (6, 2))
        let expected = None
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap1 () =
        let seg1 = (Coord (0, 0), Coord (4, 4))
        let seg2 = (Coord (1, 1), Coord (2, 2))
        let expected = Some (Overlap (0.25, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap2 () =
        let seg1 = (Coord (2, 2), Coord (6, 4))
        let seg2 = (Coord (4, 3), Coord (8, 5))
        let expected = Some (Overlap (0.5, 1.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap3 () =
        let seg1 = (Coord (9, 6), Coord (1, 2))
        let seg2 = (Coord (3, 3), Coord (5, 4))
        let expected = Some (Overlap (0.75, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectOverlap4 () =
        let seg1 = (Coord (6, 7), Coord (7, 8))
        let seg2 = (Coord (2, 3), Coord (1, 2))
        let expected = Some (Overlap (-4.0, -5.0))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint1 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (2, 0), Coord (0, 2))
        let expected = Some (Point (0.5, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint2 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (1, 0), Coord (0, 1))
        let expected = Some (Point (0.25, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)    

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint3 () =
        let seg1 = (Coord (0, 2), Coord (3, 7))
        let seg2 = (Coord (2, 0), Coord (3, 7))
        let expected = Some (Point (1.0, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)      

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint4 () =
        let seg1 = (Coord (3, 2), Coord (3, 7))
        let seg2 = (Coord (3, 7), Coord (-1, 2))
        let expected = Some (Point (1.0, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2) 

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint5 () =
        let seg1 = (Coord (4, 4), Coord (0, 0))
        let seg2 = (Coord (1, 7), Coord (5, -1))
        let expected = Some (Point (0.25, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)      

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint6 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (0, 2), Coord (2, 0))
        let expected = Some (Point (0.5, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint7 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (0, 2), Coord (-2, 4))
        let expected = Some (Point (0.5, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint8 () =
        let seg1 = (Coord (2, 2), Coord (3, 3))
        let seg2 = (Coord (-2, 2), Coord (-3, 3))
        let expected = Some (Point (-2.0, CCW))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

[<TestClass>]
type GeometryIntersectionListTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))

    [<TestMethod>]
    member this.TestSegmentItersectionListPoint1 () =
        let s1 = seg (0,0) (1,1)
        let segs =
            [
                seg (0,1) (1,0)
                seg (0,2) (1,1)
                seg (0,3) (1,2)
                seg (0,4) (1,3)
            ]
        let expected =
            [
                Point (0.5, CW)
                Point (1.0, CW)
            ]
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

    [<TestMethod>]
    member this.TestSegmentItersectionListPoint2 () =
        let s1 = seg (0,0) (-1,-1)
        let segs =
            [
                seg (1,0) (0,1)
                seg (2,0) (1,1)
                seg (3,0) (2,1)
                seg (4,0) (3,1)
            ]
        let expected : SegmentIntersect list = []
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

    [<TestMethod>]
    member this.TestSegmentItersectionListOverlapPoint () =
        let s1 = seg (0,0) (1,1)
        let segs =
            [
                seg (1,1) (2,2)
                seg (1,2) (2,3)
                seg (0,1) (1,2)
            ]
        let expected =
            [
                Overlap (1.0, 1.0)
            ]
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

    [<TestMethod>]
    member this.TestSegmentItersectionListOverlap1 () =
        let s1 = seg (0,0) (4,4)
        let segs =
            [
                seg (1,1) (2,2)
                seg (1,2) (2,3)
                seg (0,1) (1,2)
            ]
        let expected =
            [
                Overlap (0.25, 0.5)
            ]
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

    [<TestMethod>]
    member this.TestSegmentItersectionListOverlap2 () =
        let s1 = seg (1,1) (-1,-1)
        let segs =
            [
                seg (0,0) (2,2)
                seg (1,2) (2,3)
                seg (0,1) (1,2)
            ]
        let expected =
            [
                Overlap (0.0, 0.5)
            ]
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

    [<TestMethod>]
    member this.TestSegmentItersectionListOverlap3 () =
        let s1 = seg (0,0) (4,0)
        let segs =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                Point (0.25, CW)
                Overlap (0.0, 0.25)
                Point (0.0, CW)
            ]
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

[<TestClass>]
type GeometryDecomposeTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap1 () =
        let s1 = seg (0,0) (4,0)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                Overlap (0.0, 0.25)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap2 () =
        let s1 = seg (0,0) (4,0)
        let simplePolygon =
            [
                seg (0,0) (0,1)
                seg (0,1) (1,1)
                seg (1,1) (1,0)
                seg (1,0) (0,0)
            ]
        let expected =
            [
                Overlap (0.0, 0.25)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap3 () =
        let s1 = seg (-1,0) (4,0)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                Overlap (0.2, 0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap4 () =
        let s1 = seg (-1,0) (4,0)
        let simplePolygon =
            [
                seg (0,0) (0,1)
                seg (0,1) (1,1)
                seg (1,1) (1,0)
                seg (1,0) (0,0)
            ]
        let expected =
            [
                Overlap (0.2, 0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap5 () =
        let s1 = seg (0,-3) (6,6)
        let simplePolygon =
            [
                seg (0,3) (2,0)
                seg (2,0) (4,3)
                seg (4,3) (2,6)
                seg (2,6) (0,3)
            ]
        let expected =
            [
                Overlap (1.0/3.0, 2.0/3.0)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap6 () =
        let s1 = seg (6,6) (0,-3)
        let simplePolygon =
            [
                seg (0,3) (2,0)
                seg (2,0) (4,3)
                seg (4,3) (2,6)
                seg (2,6) (0,3)
            ]
        let expected =
            [
                Overlap (1.0/3.0, 2.0/3.0)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

     [<TestMethod>]
     member this.TestSegmentDecomposeOverlapMultiple1 () =
        let s1 = seg (0,-3) (10,12)
        let simplePolygon =
            [
                seg (-2,6) (2,0)
                seg (2,0) (4,3)
                seg (4,3) (2,6)
                seg (2,6) (4,9)
                seg (4,9) (6,6)
                seg (6,6) (8,9)
                seg (8,9) (4,15)
                seg (4,15) (-2,6)
            ]
        let expected =
            [
                Overlap (0.2, 0.4)
                Overlap (0.6, 0.8)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

     [<TestMethod>]
     member this.TestSegmentDecomposeOverlapMultiple2 () =
        let s1 = seg (10,12) (0,-3)
        let simplePolygon =
            [
                seg (-2,6) (2,0)
                seg (2,0) (4,3)
                seg (4,3) (2,6)
                seg (2,6) (4,9)
                seg (4,9) (6,6)
                seg (6,6) (8,9)
                seg (8,9) (4,15)
                seg (4,15) (-2,6)
            ]
        let expected =
            [
                Overlap (0.2, 0.4)
                Overlap (0.6, 0.8)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint1 () =
        let s1 = seg (-1,-1) (4,4)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                CrossPoint (0.2)
                CrossPoint (0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint2 () =
        let s1 = seg (-1,-1) (4,4)
        let simplePolygon =
            [
                seg (0,0) (0,1)
                seg (0,1) (1,1)
                seg (1,1) (1,0)
                seg (1,0) (0,0)
            ]
        let expected =
            [
                CrossPoint (0.2)
                CrossPoint (0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint3 () =
        let s1 = seg (-1,2) (4,-3)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                CrossPoint (0.2)
                CrossPoint (0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint4 () =
        let s1 = seg (-1,2) (4,-3)
        let simplePolygon =
            [
                seg (0,0) (0,1)
                seg (0,1) (1,1)
                seg (1,1) (1,0)
                seg (1,0) (0,0)
            ]
        let expected =
            [
                CrossPoint (0.2)
                CrossPoint (0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint1 () =
        let s1 = seg (-1,1) (4,-4)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                TouchPoint (0.2)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint2 () =
        let s1 = seg (-1,1) (4,-4)
        let simplePolygon =
            [
                seg (0,0) (0,1)
                seg (0,1) (1,1)
                seg (1,1) (1,0)
                seg (1,0) (0,0)
            ]
        let expected =
            [
                TouchPoint (0.2)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint3 () =
        let s1 = seg (-1,0) (4,5)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (0,0)
            ]
        let expected =
            [
                TouchPoint (0.2)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint4 () =
        let s1 = seg (-1,0) (4,5)
        let simplePolygon =
            [
                seg (0,0) (0,1)
                seg (0,1) (1,1)
                seg (1,1) (1,0)
                seg (1,0) (0,0)
            ]
        let expected =
            [
                TouchPoint (0.2)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon)