// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.Json

[<Struct>]
type Point = {
    X: int
    Y: int
}

// An interface for all types of players, either human players or AI players
type IPlayer =
    abstract member NextMove : board: Point array -> Point

type KeyboardPlayer =
    interface IPlayer with
        member this.NextMove(board: Point[]) =
            Console.WriteLine("Press a key to move")
            let newMove =
                let key = Console.ReadKey()
                match key.Key with
                | ConsoleKey.UpArrow -> { X = 0; Y = -1 }
                | ConsoleKey.DownArrow -> { X = 0; Y = 1 }
                | ConsoleKey.RightArrow -> { X = 1; Y = 0 }
                | ConsoleKey.LeftArrow -> { X = -1; Y = 0 }
                | _ -> failwith "Invalid move"
            newMove

// Define a function to construct a message to print
let from whom =
    $"from {whom}" // requires F# 5.0!

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    let p = { X = 11; Y = 22 }
    let pJSON = JsonSerializer.Serialize p
    printfn "pJSON = %s" pJSON
    let p' : Point = JsonSerializer.Deserialize pJSON
    printfn "p' =\n%A" p'
    Pathfinding.markShortestPath ()
    Pathfinding.printBoard ()
    0 // return an integer exit code