module GUI

open System

// We haven't found a good way to pass arguments through App, so we use a global
// var here.
let problemGlobalVar: Model.Problem option ref = ref None

// The stepper can't be part of the state since the state has to be equatable in
// Avalonia FuncUI.
let stepperGlobalVar: (Model.Figure -> Model.Figure) option ref = ref None

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
    let bb = Model.holeBoundingBox problem
    fun figure ->
        Penalty.penaltyEdgeLengthSqSum problem figure +
        holeBBPenalty bb figure

let stepSolver (problem: Model.Problem) =
    let rnd = System.Random (int System.DateTime.Now.Ticks)
    let neighbors = Neighbors.balancedCollectionOfNeighbors rnd
    let penalty = figurePenalty problem
    let bb = Model.holeBoundingBox problem
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

    type Tool = Select | Deselect | Move | Rotate

    type State = {
        Problem: Model.Problem
        History: ResizeArray<Model.Figure>
        Index: int // position in History
        Scale: float
        SelectedCoords: int list
        MoveFrom: (int * int) option
        Tool: Tool
    }

    let init (problem: Model.Problem) =
        {
            Problem = problem
            History = ResizeArray([problem.Figure])
            Index = 0
            Scale = 2.0
            SelectedCoords = []
            MoveFrom = None
            Tool = Select
        }

    type Msg = Forward of int | Backward of int | Reset | ZoomIn | ZoomOut | CanvasPressed of Avalonia.Point | CanvasReleased of Avalonia.Point | SelectTool of Tool

    // adds a new figure based on the current figure, if the current figure
    // is the last figure in the history
    let applyIfLast (f: Model.Figure -> Model.Figure) (state: State): State =
        if state.Index = state.History.Count - 1 then
            let newFig = f state.History.[state.Index]
            state.History.Add (newFig)
            { state with Index = state.Index + 1 }
        else state

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
            match state.Tool with
            | Select ->
                let selectedCoordIndex = findNearbyCoord x y state.History.[state.Index]
                match selectedCoordIndex with
                | None -> state
                | Some index -> { state with SelectedCoords = (index::state.SelectedCoords) |> Seq.distinct |> List.ofSeq }
            | Deselect ->
                let selectedCoordIndex = findNearbyCoord x y state.History.[state.Index]
                match selectedCoordIndex with
                | None -> state
                | Some index -> { state with SelectedCoords = List.filter (fun i -> i <> index) state.SelectedCoords }
            | Move ->
                { state with MoveFrom = Some (x, y) }
            | Rotate ->
                applyIfLast (Transformations.rotateSelectedVerticiesAround state.SelectedCoords (x, y)) state
        | CanvasReleased p ->
            match state.Tool, state.MoveFrom with
            | Move, Some (x1, y1) ->
                let x2, y2 = int(p.X / state.Scale), int(p.Y / state.Scale)
                let dx, dy = x2 - x1, y2 - y1
                applyIfLast (Transformations.translateSelectedVerticies state.SelectedCoords (dx, dy)) state
            | _ -> state
        | SelectTool tool -> { state with Tool = tool; MoveFrom = None }
    
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
                UniformGrid.create [
                    UniformGrid.dock Dock.Bottom
                    UniformGrid.columns 4
                    UniformGrid.children [
                        RadioButton.create [
                            RadioButton.content "Select"
                            RadioButton.isChecked (state.Tool == Select)
                            RadioButton.onChecked (fun _ -> dispatch (SelectTool Select))
                        ]
                        RadioButton.create [
                            RadioButton.content "Deselect"
                            RadioButton.isChecked (state.Tool == Deselect)
                            RadioButton.onChecked (fun _ -> dispatch (SelectTool Deselect))
                        ]
                        RadioButton.create [
                            RadioButton.content "Move"
                            RadioButton.isChecked (state.Tool == Move)
                            RadioButton.onChecked (fun _ -> dispatch (SelectTool Move))
                        ]
                        RadioButton.create [
                            RadioButton.content "Rotate"
                            RadioButton.isChecked (state.Tool == Rotate)
                            RadioButton.onChecked (fun _ -> dispatch (SelectTool Rotate))
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
                        if evt.Route = Avalonia.Interactivity.RoutingStrategies.Tunnel then
                            dispatch (CanvasPressed (evt.GetPosition (evt.Source :?> IVisual))))
                    Canvas.onPointerReleased (fun evt ->
                        if evt.Route = Avalonia.Interactivity.RoutingStrategies.Tunnel then
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
                            let figure = state.History.[state.Index]
                            let vs = figure.Vertices
                            state.SelectedCoords
                            |> List.map (fun i ->
                                let c = vs.[i]
                                Ellipse.create [
                                    Ellipse.left (float(c.X) * scale - (1.5 * scale))
                                    Ellipse.top (float(c.Y) * scale - (1.5 * scale))
                                    Ellipse.width (3.0 * scale)
                                    Ellipse.height (3.0 * scale)
                                    Ellipse.fill "#95a5a6"
                                ] :> Avalonia.FuncUI.Types.IView)
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
