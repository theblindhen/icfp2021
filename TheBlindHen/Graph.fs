module Graph

open Model

let adjacentVertices (fig: Figure): Map<VertexId, VertexId list> =
    let addVertex (s: VertexId) (t: VertexId) (adjMap: Map<VertexId, VertexId list>) =
        Map.change s (fun currAdj -> 
            match currAdj with
            | None -> Some [t]
            | Some currAdj -> Some (t::currAdj)) adjMap
    Array.fold (fun adjMap (s, t) -> adjMap |> addVertex s t |> addVertex t s) Map.empty fig.Edges