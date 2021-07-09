// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.Json
open ExtCore.Args

open MyLib

open FSharp.Text.Lexing

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

// Play with command line arguments
let inputName = ref "Playground/test.while"
let verbose = ref false
let warningLevel = ref 0
let compile s = printfn "Compiling %s..." s
let argSpecs =
    [ "-i", ArgType.String (fun s -> inputName := s), "Input while program"
    ; "-v", ArgType.Unit (fun () -> verbose := true), "Set verbose"
    ; "-w", ArgType.Int (fun i -> warningLevel := i), "Warning level"
    ; "--", ArgType.Rest compile, "Stop parsing command line"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo.Create(sh, ty, desc))
      |> Array.ofList

// Call the WhileParser on the file inputName
let parseWhile file =
  let source = System.IO.File.ReadAllText(file)
  let lexbuf = LexBuffer<char>.FromString source
  WhileParser.start WhileLexer.token lexbuf
  
[<EntryPoint>]
let main argv =
    ArgParser.Parse(argSpecs, compile)
    let message = from "F#" // Call local function
    printfn $"Arguments parsed were\n\tinputName={!inputName}\n\tverbose={!verbose}\n\twarningLevel={!warningLevel}"
    
    // Parsing While language using Lex + Yacc
    printfn $"Parsed While program:\n{parseWhile !inputName}"
    
    
    // Some Json
    let p = { X = 11; Y = 22 }
    let pJSON = JsonSerializer.Serialize p
    printfn "pJSON = %s" pJSON
    let p' : Point = JsonSerializer.Deserialize pJSON
    printfn "p' =\n%A" p'

    // Pathfinding on the board
    Pathfinding.markShortestPathAStar ()
    Pathfinding.printBoard ()

    Say.libHello "console program monkey"
    0 // return an integer exit code