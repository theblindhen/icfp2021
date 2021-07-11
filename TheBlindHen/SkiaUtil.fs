module SkiaUtil

open Model

let holeAsPath (hole: Coord array) =
    let path = new SkiaSharp.SKPath()
    path.MoveTo(float32 hole.[0].X, float32 hole.[0].Y)
    for i in 1 .. hole.Length - 1 do
        path.LineTo(float32 hole.[i].X, float32 hole.[i].Y)
    path.Close()
    path

let isInHole (hole: Coord array) =
    let hole = holeAsPath hole
    fun (c: Coord) -> hole.Contains(float32 c.X, float32 c.Y)