module Neighbors

open Model

let translateRandomCoord (rnd: System.Random) (solution: Solution) =
    let rndIndex = rnd.Next(solution.SolutionVertices.Length)
    List.map (fun (dx, dy) -> 
        let rndCord = Array.item rndIndex solution.SolutionVertices
        let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
        let newSolution = copySolution solution
        Array.set newSolution.SolutionVertices rndIndex newCord
        newSolution) [(1, 0); (0, 1); (-1, 0); (0, -1)]