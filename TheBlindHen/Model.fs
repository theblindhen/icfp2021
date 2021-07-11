module Model

open System
open System.Text.Json

// This module contains the raw types that can be automatically read by the JSON
// serializer/deserializer.
module Raw =
    type Figure = {
        edges: int array array
        vertices: int array array
    }

    type Problem = {
        hole: int array array
        figure: Figure
        epsilon: int
    }
    
    type Solution = {
        vertices: int array array
    }

    let parseString (str: string): Problem =
        JsonSerializer.Deserialize str

    let parseFile (file: string): Problem =
        parseString (System.IO.File.ReadAllText file)

    let parseSolutionFile (file: string): Solution =
        System.IO.File.ReadAllText file
        |> JsonSerializer.Deserialize

// The following types are for general use in the program. They should be
// general enough to support anything we might want to do with problems and
// solutions but specific enough that they don't admit obviously ill-formed
// data.

type Coord = 
    struct 
        val X: int
        val Y: int
        new(x: int, y: int) = { X = x; Y = y }

        // member this.GetDistanceFrom(p: Coordinate) = 
        //     let dX = pown (p.X - this.X) 2
        //     let dY = pown (p.Y - this.Y) 2
            
        //     dX + dY
        //     |> sqrt
        override c.ToString() = $"({c.X},{c.Y})"
    end

type VertexId = int

type Figure = {
    Edges: (VertexId * VertexId) array
    Vertices: Coord array
}

type Problem = {
    Hole: Coord array
    Figure: Figure
    Epsilon: int
}

type Solution = {
    SolutionVertices: Coord array
}

let solutionOfFigure (f: Figure): Solution =
    {
        SolutionVertices = f.Vertices
    }

let mapFigureVerticies (f : Coord -> Coord) (fig: Figure): Figure =
    { fig with Vertices = Array.map f fig.Vertices }

let mapiFigureVerticies (f : int -> Coord -> Coord) (fig: Figure): Figure =
    { fig with Vertices = Array.mapi f fig.Vertices }

let copyFigureVerticies (fig: Figure): Figure =
    { fig with Vertices = Array.copy fig.Vertices }

let copySolution (s: Solution): Solution =
    {
        SolutionVertices = Array.copy s.SolutionVertices
    }

let figureOfSolution (problem: Problem) (s: Solution): Figure =
    let fig = copyFigureVerticies problem.Figure
    { problem.Figure with Vertices = s.SolutionVertices }


let private fromRawFigure (raw: Raw.Figure) : Figure =
    {
        Edges = raw.edges |> Array.map (fun xy -> (xy.[0], xy.[1]))
        Vertices = raw.vertices |> Array.map (fun xy -> Coord(xy.[0], xy.[1]))
    }

let private fromRawProblem (raw: Raw.Problem) : Problem =
    {
        Hole = raw.hole |> Array.map (fun xy -> Coord(xy.[0], xy.[1]))
        Figure = raw.figure |> fromRawFigure
        Epsilon = raw.epsilon
    }

let private fromRawSolution (raw: Raw.Solution) : Solution =
    {
        SolutionVertices = raw.vertices |> Array.map (fun xy -> Coord(xy.[0], xy.[1]))
    }

let parseString (str: string) : Problem =
    fromRawProblem (Raw.parseString str)

let parseFile (file: string) : Problem =
    fromRawProblem (Raw.parseFile file)

let parseSolutionFile (file: string) : Solution =
    fromRawSolution (Raw.parseSolutionFile file)

let deparseSolution (sol: Solution) : string =
    """{"vertices":[""" + String.concat "," (sol.SolutionVertices |> Array.map (fun c -> $"[{c.X},{c.Y}]")) + """]}"""

let holeSegments (problem: Problem) =
    match Array.tryLast problem.Hole with
    | None -> []
    | Some last ->
        problem.Hole
        |> Array.fold (fun (edges, last) cur ->
            ((last, cur)::edges, cur)) ([], last)
        |> fst
        |> List.rev

let figureSegments (fig: Figure) =
    fig.Edges
    |> Array.toList
    |> List.map (fun (v1, v2) -> (fig.Vertices.[v1], fig.Vertices.[v2]))

/// Returns (smallest, largest)
let holeBoundingBox (problem: Problem) =
    let xs = problem.Hole |> Array.map (fun c -> c.X)
    let ys = problem.Hole |> Array.map (fun c -> c.Y)
    let minx = xs |> Array.min
    let miny = ys |> Array.min
    let maxx = xs |> Array.max
    let maxy = ys |> Array.max
    (Coord (minx, miny), Coord (maxx, maxy))

/// Returns (smallest, largest)
let figureBoundingBox (figure: Figure) =
    let xs = figure.Vertices |> Array.map (fun c -> c.X)
    let ys = figure.Vertices |> Array.map (fun c -> c.Y)
    let minx = xs |> Array.min
    let miny = ys |> Array.min
    let maxx = xs |> Array.max
    let maxy = ys |> Array.max
    (Coord (minx, miny), Coord (maxx, maxy))
