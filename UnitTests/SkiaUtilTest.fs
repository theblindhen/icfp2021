namespace UnitTests

open System
open Model
open Microsoft.VisualStudio.TestTools.UnitTesting

open SkiaUtil

[<TestClass>]
type SkiaUtilTestClass () =

    // Helpers
    let toHole xys =
        xys |> List.map (fun (x,y) -> Coord(x,y)) |> Array.ofList
    let testHole1 = toHole [ (1, 1) ; (3, 1) ; (3, 3) ; (1, 3) ]
    let testHole2 = toHole [ (2, 0); (1, 1); (2, 2); (3, 1); ]
    let wedgeHole = toHole [ (70,90) ; (50,95) ; (30,90) ; (30, 0) ; (70,0)  ; ]
    let longTriangle = toHole [ (0, 0); (0, 2); (200, 0) ]
    let thinWedge = toHole [ (0, 0); (1, 2000); (2, 0) ]

    [<TestMethod>]
    member this.TestIsInHole1 () = 
        Assert.IsTrue(isInHole testHole1 (Coord(1,1)))

    [<TestMethod>]
    member this.TestIsInHole2 () = 
        Assert.IsTrue(isInHole testHole1 (Coord(2,2)))

    [<TestMethod>]
    member this.TestIsInHole3 () = 
        Assert.IsTrue(isInHole testHole1 (Coord(3,3)))

    [<TestMethod>]
    member this.TestIsInHole4 () = 
        Assert.IsFalse(isInHole testHole1 (Coord(-1,3)))

    [<TestMethod>]
    member this.TestIsInHole5 () = 
        Assert.IsFalse(isInHole testHole1 (Coord(4,4)))

    [<TestMethod>]
    member this.TestIsInHole6 () = 
        Assert.IsTrue(isInHole testHole2 (Coord(1,1)))

    [<TestMethod>]
    member this.TestIsInHole7 () = 
        Assert.IsTrue(isInHole testHole2 (Coord(2,2)))

    [<TestMethod>]
    member this.TestIsInHole8 () = 
        Assert.IsTrue(isInHole testHole2 (Coord(2,0)))

    [<TestMethod>]
    member this.TestIsInHole9 () = 
        Assert.IsFalse(isInHole testHole2 (Coord(3,0)))

    [<TestMethod>]
    member this.TestIsInHole10 () = 
        Assert.IsFalse(isInHole testHole2 (Coord(3,3)))

    [<TestMethod>]
    member this.TestIsInHole11 () = 
        Assert.IsTrue(isInHole wedgeHole (Coord(50,95)))

    [<TestMethod>]
    member this.TestOnEdge () = 
        Assert.IsTrue(isInHole longTriangle (Coord(100,1)))
        Assert.IsTrue(isInHole longTriangle (Coord(99,1)))
        Assert.IsFalse(isInHole longTriangle (Coord(99,2)))
        Assert.IsFalse(isInHole longTriangle (Coord(101,1)))
        Assert.IsTrue(isInHole longTriangle (Coord(101,0)))

    [<TestMethod>]
    member this.TestCloseToEdge2 () = 
        Assert.IsFalse(isInHole thinWedge (Coord(0,1999)))
        Assert.IsTrue(isInHole thinWedge (Coord(1,1999)))
        Assert.IsFalse(isInHole thinWedge (Coord(2,1999)))
        Assert.IsTrue(isInHole thinWedge (Coord(1,2000)))
        Assert.IsFalse(isInHole thinWedge (Coord(1,2001)))

    [<TestMethod>]
    member this.TestCloseToEdge3 () = 
        let triangleWithIntPts = 
            toHole [ (0, 0); (0, 7 * 59); (7 * 97, 0) ]
        for i in [0..7] do
            let pnt = Coord(i*97, (7-i)*59)
            Assert.IsTrue(isInHole triangleWithIntPts pnt)
            Assert.IsFalse(isInHole triangleWithIntPts (Coord (pnt.X, pnt.Y+1)))
            Assert.IsFalse(isInHole triangleWithIntPts (Coord (pnt.X+1, pnt.Y)))
            Assert.IsFalse(isInHole triangleWithIntPts (Coord (pnt.X+1, pnt.Y+1)))
