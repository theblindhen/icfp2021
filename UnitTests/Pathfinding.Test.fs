namespace Pathfinding.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

  [<TestMethod>]
  member this.Array2DSequence () =
    let arr = Array2D.init 2 2 (+)
    let res = Pathfinding.array2DSequence arr 
              |> List.ofSeq
    Assert.AreEqual (res, [ (0,0,0); (0,1,1); (1,0,1); (1,1,2) ])
