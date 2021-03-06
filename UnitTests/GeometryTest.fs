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
     member this.TestSegmentIntersectOverlap5 () =
        let seg1 = seg (0,-1) (0,3)
        let seg2 = seg (0,0) (0,1)
        let expected = Some (Overlap (0.25, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)       

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint1 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (2, 0), Coord (0, 2))
        let expected = Some (Point (0.5, CCW, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint2 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (1, 0), Coord (0, 1))
        let expected = Some (Point (0.25, CCW, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)    

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint3 () =
        let seg1 = (Coord (0, 2), Coord (3, 7))
        let seg2 = (Coord (2, 0), Coord (3, 7))
        let expected = Some (Point (1.0, CCW, 1.0))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)      

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint4 () =
        let seg1 = (Coord (3, 2), Coord (3, 7))
        let seg2 = (Coord (3, 7), Coord (-1, 2))
        let expected = Some (Point (1.0, CCW, 0.0))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2) 

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint5 () =
        let seg1 = (Coord (4, 4), Coord (0, 0))
        let seg2 = (Coord (1, 7), Coord (5, -1))
        let expected = Some (Point (0.25, CCW, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)      

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint6 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (0, 2), Coord (2, 0))
        let expected = Some (Point (0.5, CW, 0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint7 () =
        let seg1 = (Coord (0, 0), Coord (2, 2))
        let seg2 = (Coord (0, 2), Coord (-2, 4))
        let expected = Some (Point (0.5, CCW, -0.5))
        Assert.AreEqual(expected, segmentsIntersect seg1 seg2)

    [<TestMethod>]
    member this.TestSegmentsIntersectPoint8 () =
        let seg1 = (Coord (2, 2), Coord (3, 3))
        let seg2 = (Coord (-2, 2), Coord (-3, 3))
        let expected = Some (Point (-2.0, CCW, -2.0))
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
                0, Point (0.5, CW, 0.5)
                1, Point (1.0, CW, 1.)
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
        let expected : (int * SegmentIntersect) list = []
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

    [<TestMethod>]
    member this.TestSegmentIntersectionListPoint3 () =
        let s1 = seg (-1,1) (4,-4)
        let simplePolygon =
            [
                seg (1,1) (1,0)
                seg (1,0) (0,0)
                seg (0,0) (0,1)
                seg (0,1) (1,1)
            ]
        let expected = [
            1, Point (0.2, CW, 1.0)
            2, Point (0.2, CCW, 0.0)
        ]
        Assert.AreEqual(expected, segmentIntersectionList s1 simplePolygon)

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
                0, Overlap (1.0, 1.0)
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
                0, Overlap (0.25, 0.5)
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
                0, Overlap (0.0, 0.5)
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
                0, Point (0.25, CW, 1.0)
                1, Overlap (0.0, 0.25)
                2, Point (0.0, CW, 1.0)
            ]
        Assert.AreEqual(expected, segmentIntersectionList s1 segs)

[<TestClass>]
type GeometryDecomposeTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))

    let simplePolygon1 =
        [
            seg (1,1) (1,0)
            seg (1,0) (0,0)
            seg (0,0) (0,1)
            seg (0,1) (1,1)
        ]

    let simplePolygon2 =
        [
            seg (0,0) (0,1)
            seg (0,1) (1,1)
            seg (1,1) (1,0)
            seg (1,0) (0,0)
        ]

    let simplePolygon3 =
        [
            seg (0,3) (2,0)
            seg (2,0) (4,3)
            seg (4,3) (2,6)
            seg (2,6) (0,3)
        ]
    let simplePolygon4 =
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
    let simplePolygon5 =
        [
            seg (1,1) (3,2)
            seg (3,2) (2,4)
            seg (2,4) (6,5)
            seg (6,5) (6,1)
            seg (6,1) (1,1)
        ]
    let bigbox =
        [ 
            seg (0,0) (0,3)
            seg (0,3) (3,3)
            seg (3,3) (3,0)
            seg (3,0) (0,0)
        ]
    let stairs =
        [
            seg (0,0) (1,1)
            seg (1,1) (3,1)
            seg (3,1) (5,3)
            seg (5,3) (5,0)
            seg (5,0) (0,0)
        ]
    let saddle =
        [
            seg (0,0) (1,1)
            seg (1,1) (4,1)
            seg (4,1) (5,0)
            seg (5,0) (0,0)
        ]

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap1 () =
        let s1 = seg (0,0) (4,0)
        let expected =
            [
                DecPoint (0.0, Touch)
                DecOverlap (0.0, 0.25)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon1)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap2 () =
        let s1 = seg (0,0) (4,0)
        let expected =
            [
                DecPoint (0.0, Touch)
                DecOverlap (0.0, 0.25)                
            ]
        let out = segmentDecomposition s1 simplePolygon2
        Assert.AreEqual(expected, out)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap3 () =
        let s1 = seg (-1,0) (4,0)
        let expected =
            [
                DecPoint (0.2, Touch)
                DecOverlap (0.2, 0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon1)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap4 () =
        let s1 = seg (-1,0) (4,0)
        let expected =
            [
                DecPoint (0.2, Touch)
                DecOverlap (0.2, 0.4)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon2)

    [<TestMethod>]
    member this.TestSegmentDecomposeOverlap5 () =
        let s1 = seg (0,-3) (6,6)
        let expected =
            [
                DecOverlap (1.0/3.0, 2.0/3.0)                
                DecPoint (2.0/3.0, Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon3)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap6 () =
        let s1 = seg (6,6) (0,-3)
        let expected =
            [
                DecPoint (1.0/3.0, Touch)
                DecOverlap (1.0/3.0, 2.0/3.0)            
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon3)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap7 () =
        let s1 = seg (0,1) (4,1)
        let expected =
            [
                DecOverlap (0.25, 0.75)            
                DecPoint (0.75, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 stairs)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap8 () =
        let s1 = seg (1,1) (4,1)
        let expected =
            [
                DecOverlap (0., 2./3.)            
                DecPoint (2./3., Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 stairs)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap9 () =
        let s1 = seg (2,1) (4,1)
        let expected =
            [
                DecOverlap (0., 0.5)            
                DecPoint (0.5, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 stairs)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap10 () =
        let s1 = seg (0,1) (3,1)
        let expected =
            [
                DecOverlap (1./3., 1.)            
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 saddle)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap11 () =
        let s1 = seg (1,1) (4,1)
        let expected =
            [
                DecOverlap (0., 1.)            
                DecPoint (1., Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 saddle)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap12 () =
        let s1 = seg (2,1) (4,1)
        let expected =
            [
                DecOverlap (0., 1.)            
                DecPoint (1., Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 saddle)
 
     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap13 () =
        let s1 = seg (2,1) (3,1)
        let expected =
            [
                DecOverlap (0., 1.0)            
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 saddle)

     [<TestMethod>]
     member this.TestSegmentDecomposeOverlap14 () =
        let s1 = seg (20,-20) (20,0)
        let segs = [
            seg (20, 0) (20, 20)
            seg (20, 20) (0, 20)
            seg (0, 20) (20, 0)
        ]
        let expected =
            [
                DecOverlap (1.0, 1.0)            
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 segs)


     [<TestMethod>]
     member this.TestSegmentDecomposeOverlapMultiple1 () =
        let s1 = seg (0,-3) (10,12)
        let expected =
            [
                DecOverlap (0.2, 0.4)            
                DecPoint (0.4, Touch)
                DecOverlap (0.6, 0.8)       
                DecPoint (0.8, Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon4)

     [<TestMethod>]
     member this.TestSegmentDecomposeOverlapMultiple2 () =
        let s1 = seg (10,12) (0,-3)
        let expected =
            [
                DecPoint (0.2, Touch)
                DecOverlap (0.2, 0.4)            
                DecPoint (0.6, Touch)
                DecOverlap (0.6, 0.8)       
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon4)

     [<TestMethod>]
     member this.TestSegmentDecomposeOverlapMultiple3 () =
        let s1 = seg (0,-1) (0,3)
        let tria = [ seg (0,0) (0,1)
                     seg (0,1) (0,2)
                     seg (0,2) (1,2)
                     seg (1,2) (0,0) ]
        let expected =
            [
                DecOverlap (0.25, 0.5)            
                DecOverlap (0.5, 0.75)            
                DecPoint (0.75, Touch)            
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 tria)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint1 () =
        let s1 = seg (-1,-1) (4,4)
        let expected =
            [
                DecPoint (0.2, Cross)
                DecPoint (0.4, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon1)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint2 () =
        let s1 = seg (-1,-1) (4,4)
        let expected =
            [
                DecPoint (0.2, Cross)
                DecPoint (0.4, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon2)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint3 () =
        let s1 = seg (-1,2) (4,-3)
        let expected =
            [
                DecPoint (0.2, Cross)
                DecPoint (0.4, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon1)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint4 () =
        let s1 = seg (-1,2) (4,-3)
        let expected =
            [
                DecPoint (0.2, Cross)
                DecPoint (0.4, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon2)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint5 () =
        let s1 = seg (-2,-2) (2,2)
        let expected = [ DecPoint (0.5, Cross) ]
        Assert.AreEqual(expected, segmentDecomposition s1 bigbox)

    [<TestMethod>]
    member this.TestSegmentDecomposeCrossPoint6 () =
        let s1 = seg (0,-2) (2,2)
        let expected = [ DecPoint (0.5, Cross) ]
        Assert.AreEqual(expected, segmentDecomposition s1 bigbox)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint1 () =
        let s1 = seg (-1,1) (4,-4)
        let expected =
            [
                DecPoint (0.2, Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon1)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint2 () =
        let s1 = seg (-1,1) (4,-4)
        let expected =
            [
                DecPoint (0.2, Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon2)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint3 () =
        let s1 = seg (-1,0) (4,5)
        let expected =
            [
                DecPoint (0.2, Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon1)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint4 () =
        let s1 = seg (-1,0) (4,5)
        let expected =
            [
                DecPoint (0.2, Touch)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon2)

    [<TestMethod>]
    member this.TestSegmentDecomposeTouchPoint5 () =
        let s1 = seg (-1,0) (5,3)
        let expected =
            [
                DecOverlap (1.0/3.0, 2.0/3.0)
                DecPoint (2.0/3.0, Cross)
            ]
        Assert.AreEqual(expected, segmentDecomposition s1 simplePolygon5)

[<TestClass>]
type GeometryGraphTestClass () =

    // Helpers
    let seg (x1,y1) (x2,y2) = (Coord (x1,y1), Coord (x2,y2))

    [<TestMethod>]
    member this.TestArticulationPoints1 () =
        let dummy = Coord (0,0)
        let figure : Figure = 
            {
                Edges = [| (0,5); (0,1); (1,2); (1,3); (2,3); (3,4); (2,4) |]
                Vertices = Array.create 6 dummy
            }
        let expected = [ (0,true); (1,true); (2,false); (3,false); (4,false); (5,false)]
        let (adj, ap) = Graph.getArticulationPoints figure
        Assert.AreEqual(expected, List.map (fun n -> (n,ap.[n])) [0..5])

    [<TestMethod>]
    member this.TestArticulationPoints2 () =
        let dummy = Coord (0,0)
        let figure : Figure = 
            {
                Edges = [| (0,5); (0,1); (5,2); (1,2); (1,3); (2,3); (3,4); (2,4) |]
                Vertices = Array.create 6 dummy
            }
        let expected = [ (0,false); (1,false); (2,false); (3,false); (4,false); (5,false)]
        let (adj, ap) = Graph.getArticulationPoints figure
        Assert.AreEqual(expected, List.map (fun n -> (n,ap.[n])) [0..5])

    [<TestMethod>]
    member this.TestArticulationPoints3 () =
        let dummy = Coord (0,0)
        let figure : Figure = 
            {
                Edges = [| (0,1); (0,2); (1,3); (2,3); (3,4); (3,5); (4,6); (5,6) |]
                Vertices = Array.create 7 dummy
            }
        let expected = [ (0,false); (1,false); (2,false); (3,true); (4,false); (5,false); (6,false)]
        let (adj, ap) = Graph.getArticulationPoints figure
        Assert.AreEqual(expected, List.map (fun n -> (n,ap.[n])) [0..6])