module Graph

open Model

let adjacentVertices (fig: Figure): Map<VertexId, VertexId list> =
    let addVertex (s: VertexId) (t: VertexId) (adjMap: Map<VertexId, VertexId list>) =
        Map.change s (fun currAdj -> 
            match currAdj with
            | None -> Some [t]
            | Some currAdj -> Some (t::currAdj)) adjMap
    Array.fold (fun adjMap (s, t) -> adjMap |> addVertex s t |> addVertex t s) Map.empty fig.Edges

let getArticulationPoints (figure : Figure) =
    let adj : bool[,] = Array2D.create (figure.Vertices.Length) (figure.Vertices.Length) false
    let disc : int array = Array.zeroCreate (figure.Vertices.Length)
    let low : int array = Array.create (figure.Vertices.Length) (System.Int32.MaxValue)
    let visited : bool array = Array.create (figure.Vertices.Length) false
    let parent : VertexId array = Array.create (figure.Vertices.Length) (-1)
    let articulationPoint : bool array = Array.create (figure.Vertices.Length) false

    for (a,b) in figure.Edges do
        adj.[a,b] <- true
        adj.[b,a] <- true

    let rec dfs (vertex : VertexId) (time: int) =
        visited.[vertex] <- true
        disc.[vertex] <- time+1
        low.[vertex] <- time+1
        let mutable child = 0
        for i in 0..figure.Vertices.Length - 1 do
            if adj.[vertex,i] then
                if not visited.[i] then
                    child <- child + 1
                    parent.[i] <- vertex
                    dfs i (time + 1)
                    low.[vertex] <- min low.[vertex] low.[i]
                    if parent.[vertex] = -1 && child > 1 then
                        articulationPoint.[vertex] <- true
                    if parent.[vertex] <> -1 && low.[i] >= disc.[vertex] then
                        articulationPoint.[vertex] <- true
                else if parent.[vertex] <> i then
                    low.[vertex] <- min low.[vertex] disc.[i]
    dfs 0 0
    (adj, articulationPoint)


let connectedComponentsWithoutVertex (v: VertexId) (adj: bool[,]) =
    let noVertices = Array2D.length1 adj
    let visited : bool array = Array.create noVertices false
    let mutable components: Map<int, VertexId list> = Map.empty

    let rec dfs v componentIdx =
        visited.[v] <- true
        for i in 0..noVertices - 1 do
            if adj.[v,i] && not(visited.[i]) then
                components <- Map.add componentIdx (i::Map.find componentIdx components) components
                dfs i componentIdx

    visited.[v] <- true
    for i in 0..noVertices - 1 do
        if adj.[v,i] && not(visited.[i]) then
            components <- Map.add i [i] components
            dfs i i

    Map.values components
    |> Set.toList
