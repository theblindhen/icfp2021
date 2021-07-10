module GUI

open System

// We haven't found a good way to pass arguments through App, so we use a global
// var here.
let problemGlobalVar: Model.Problem option ref = ref None

// The stepper can't be part of the state since the state has to be equatable in
// Avalonia FuncUI.
let stepperGlobalVar: (Model.Figure -> Model.Figure) option ref = ref None

/// Returns (smallest, largest)
let holeBoundingBox (problem: Model.Problem) =
    let segments = Model.holeSegments problem
    let xs = segments |> List.collect (fun (xy1, xy2) -> [xy1.X; xy2.X])
    let ys = segments |> List.collect (fun (xy1, xy2) -> [xy1.Y; xy2.Y])
    let minx = xs |> List.min
    let miny = ys |> List.min
    let maxx = xs |> List.max
    let maxy = ys |> List.max
    (Model.Coord (minx, miny), Model.Coord (maxx, maxy))

let holeBBPenalty (minCorner: Model.Coord, maxCorner: Model.Coord) (figure: Model.Figure) =
    figure.Vertices
    |> Array.sumBy (fun xy ->
        max 0 (minCorner.X - xy.X) +
        max 0 (xy.X - maxCorner.X) +
        max 0 (minCorner.Y - xy.Y) +
        max 0 (xy.Y - maxCorner.Y))
    |> float
    |> ( * ) 1000.0

let figurePenalty (problem: Model.Problem) =
    let bb = holeBoundingBox problem
    fun figure ->
        Penalty.penaltyEdgeLengthSqSum problem figure +
        holeBBPenalty bb figure

let stepSolver (problem: Model.Problem) =
    let rnd = System.Random (int System.DateTime.Now.Ticks)
    let neighbors = Neighbors.translateRandomCoord rnd
    let penalty = figurePenalty problem
    let bb = holeBoundingBox problem
    fun figure ->
        let result = Hillclimber.step neighbors penalty figure
        // TODO: this is just debug printing
        printfn "penalty = %f + %f"
            (Penalty.penaltyEdgeLengthSqSum problem result)
            (holeBBPenalty bb result)
        result

let findNearbyCoord x y (figure: Model.Figure) =
    let dist (coord: Model.Coord) =
        let dx, dy = abs (coord.X - x), abs (coord.Y - y)
        dx+dy
    let nearestPoint = Array.minBy dist figure.Vertices
    if dist nearestPoint < 5 then
        Array.tryFindIndex (fun c -> c = nearestPoint) figure.Vertices
    else None

module MVU =
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Avalonia.VisualTree
    open Avalonia.Controls.Shapes

    type State = {
        Problem: Model.Problem
        History: ResizeArray<Model.Figure>
        Index: int // position in History
        Scale: float
        SelectedCoordIndex: int option
    }

    let init (problem: Model.Problem) =
        let mangledFigure = Model.copyFigure problem.Figure
        //mangledFigure.Vertices.[1] <- Model.Coord(12, 15) // TODO: remove this test code!
        //mangledFigure.Vertices.[2] <- Model.Coord(25, 20) // TODO: remove this test code!
        {
            Problem = problem
            History = ResizeArray([mangledFigure])
            Index = 0
            Scale = 2.0
            SelectedCoordIndex = None
        }

    type Msg = Forward of int | Backward of int | Reset | ZoomIn | ZoomOut | CanvasPressed of Avalonia.Point | CanvasReleased of Avalonia.Point

    let update (msg: Msg) (state: State) : State =
        match msg with
        | Forward steps ->
            let newIndex = state.Index + steps
            let stepper = Option.get !stepperGlobalVar
            while newIndex >= state.History.Count do
                let lastState = state.History.[state.History.Count - 1]
                state.History.Add (stepper lastState)
            { state with Index = newIndex }
        | Backward steps -> { state with Index = max 0 (state.Index - steps) }
        | Reset -> { state with Index = 0 }
        | ZoomIn -> { state with Scale = state.Scale * 1.50 }
        | ZoomOut -> { state with Scale = state.Scale / 1.50 }
        | CanvasPressed p ->
            let x, y = int(p.X / state.Scale), int(p.Y / state.Scale)
            let selectedCoordIndex = findNearbyCoord x y state.History.[state.Index]
            { state with SelectedCoordIndex = selectedCoordIndex }
        | CanvasReleased p ->
            match state.SelectedCoordIndex, state.Index = state.History.Count - 1 with
            | Some selected, true ->
                let x, y = int(p.X / state.Scale), int(p.Y / state.Scale)
                let newFigure = Model.copyFigure state.History.[state.Index]
                newFigure.Vertices.[selected] <- Model.Coord(x, y)
                state.History.Add (newFigure)
                { state with SelectedCoordIndex = None; Index = state.Index + 1 }
            | _ -> state
    
    let view (state: State) (dispatch) =
        let scale = state.Scale
        DockPanel.create [
            DockPanel.children [
                Button.create [
                    Button.dock Dock.Bottom
                    Button.onClick (fun _ -> dispatch Reset)
                    Button.content "reset"
                ]                
                UniformGrid.create [
                    UniformGrid.dock Dock.Bottom
                    UniformGrid.columns 2
                    UniformGrid.children [
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Backward 1))
                            Button.content "<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Forward 1))
                            Button.content ">"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Backward 10))
                            Button.content "<<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Forward 10))
                            Button.content ">>"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Backward 100))
                            Button.content "<<<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Forward 100))
                            Button.content ">>>"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch ZoomOut)
                            Button.content "-"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch ZoomIn)
                            Button.content "+"
                        ]
                    ]
                ]
                TextBox.create [
                    TextBox.dock Dock.Bottom
                    TextBox.text (sprintf $"Step: {state.Index}, Cost: {figurePenalty state.Problem state.History.[state.Index]}")
                ]
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.onPointerPressed (fun evt ->
                        dispatch (CanvasPressed (evt.GetPosition (evt.Source :?> IVisual))))
                    Canvas.onPointerReleased (fun evt ->
                        dispatch (CanvasReleased (evt.GetPosition (evt.Source :?> IVisual))))
                    Canvas.children (
                        (
                            let figure = state.History.[state.Index]
                            let vs = figure.Vertices
                            figure.Edges
                            |> Array.toList
                            |> List.map (fun (s,t) ->
                                Line.create [
                                    Line.startPoint (float vs.[s].X * scale, float vs.[s].Y * scale)
                                    Line.endPoint (float vs.[t].X * scale, float vs.[t].Y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#e74c3c"
                                ] :> Avalonia.FuncUI.Types.IView
                            )
                        ) @
                        (
                            Model.holeSegments state.Problem
                            |> List.map (fun (s,t) ->
                                Line.create [
                                    Line.startPoint (float s.X * scale, float s.Y * scale)
                                    Line.endPoint (float t.X * scale, float t.Y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#000000"
                                ] :> Avalonia.FuncUI.Types.IView
                            )
                        )
                    )
                ]       
            ]
        ]       

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "TestUI"
        base.Width <- 400.0
        base.Height <- 400.0
        
        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let problem = Option.get !problemGlobalVar

        Elmish.Program.mkSimple MVU.init MVU.update MVU.view
        |> Program.withHost this
        |> Program.runWith problem

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

let showGui problem =
        problemGlobalVar := Some problem 
        stepperGlobalVar := Some (stepSolver problem)
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime([||]) // TODO: how to parse args
