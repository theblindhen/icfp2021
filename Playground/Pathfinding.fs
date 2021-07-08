module Pathfinding

open System

// In this map, `s` marks the start of a path, and `f` marks the end of a path.
let boardString = """
    ################################################
    ###                     ##                    f#
    ###                     ##  ####################
    ###                     #              #########
    ###                     ########  ###         ##
    ###                     ##############        ##
    #                             ############# ####
    #                             ####     #### ####
    #                                      #### ####
    #                             #        #       #
    #                             #        #       #
    #                             #                #
    #                             #        #       #
    #                                      #       #
    #                             #        #       #
    #                             #        #       #
    #                                      #### ####
    #                             #        ###   ###
    #s                            #        #########
    ################################################
    """

[<Struct>]
type Vec = {
    X: int
    Y: int
}

// Convert the board into a 2D array of characters.
let boardArray2D =
    let lines =
        boardString.Split('\n')
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun line -> line.Length > 0)
    let width = lines.[0].Length
    let height = lines.Length
    Array2D.init width height (fun x y -> lines.[y].[x])

let array2DSequence array2D = 
    seq {
        for x in [0..(Array2D.length1 array2D) - 1] do 
              for y in [0..(Array2D.length2 array2D) - 1] do 
                  yield (x, y, array2D.[x, y])
    }

// Finds the first index pair that matches the given predicate.
let array2Dfind predicate array =
    Seq.find
        (fun (_, _, value) -> predicate value)
        (array2DSequence array)

let markShortestPath () =
    let width = Array2D.length1 boardArray2D
    let height = Array2D.length2 boardArray2D
    let (startx, starty, _) = array2Dfind (fun c -> c = 's') boardArray2D
    let (finishx, finishy, _) = array2Dfind (fun c -> c = 'f') boardArray2D
    // The predecessor map should probably be an array, but I'm trying a HashSet
    // just to explore the API.
    let predecessor = System.Collections.Generic.Dictionary()
    let rec searchBFSFrontier frontier =
        match Seq.isEmpty frontier with
        | true -> None
        | false ->
        match Seq.tryFind (fun (x, y) -> boardArray2D.[x, y] = 'f') frontier with
        | Some (x, y) -> Some (x, y)
        | None ->
        let frontier' =
            frontier
            |> List.collect (fun (x, y) ->
                let neighbors =
                    [(x+1, y); (x,y+1); (x-1, y); (x, y-1)]
                    |> List.filter (fun (x',y') -> boardArray2D.[x',y'] <> 's')
                    |> List.filter (fun (x',y') -> boardArray2D.[x',y'] <> '#')
                    |> List.filter (predecessor.ContainsKey >> not)
                neighbors
                |> List.iter (fun xy' -> predecessor.Add(xy', (x, y)))
                neighbors
            )
        searchBFSFrontier frontier'
    let rec markPredFrom (x, y) =
        match predecessor.TryGetValue((x, y)) with
        | false, _ -> ()
        | true, (x', y') ->
            boardArray2D.[x', y'] <- 'o'
            markPredFrom (x', y')
    printfn "Starting search"
    match searchBFSFrontier [(startx, starty)] with
    | None -> printfn "Not found"
    | Some (x, y) ->
    printfn "Reconstructing path"
    markPredFrom (x, y)

let printBoard () =
    for y in 0 .. Array2D.length2 boardArray2D - 1 do
        for x in 0 .. Array2D.length1 boardArray2D - 1 do
            printf "%c" boardArray2D.[x, y]
        printfn ""
