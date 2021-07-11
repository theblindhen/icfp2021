module GUI

open System

let WINDOWSIZE = 800.0

// We haven't found a good way to pass arguments through App, so we use a global
// var here.
let problemGlobalVar: Model.Problem option ref = ref None

// The stepper can't be part of the state since the state has to be equatable in
// Avalonia FuncUI.
let stepperGlobalVar: ((Model.Figure * float) -> (Model.Figure * float)) option ref = ref None

let writeSolutionGlobalVar: (Model.Figure -> unit) option ref = ref None

let findNearbyCoord (c: Model.Coord) (figure: Model.Figure) =
    let dist (coord: Model.Coord) =
        let dx, dy = abs (coord.X - c.X), abs (coord.Y - c.Y)
        dx+dy
    let nearestPoint = Array.minBy dist figure.Vertices
    if dist nearestPoint < 5 then
        Array.tryFindIndex (fun c -> c = nearestPoint) figure.Vertices
    else None

let stepSolverWithStopAndDebug problem =
    let stepSolver = FitInHole.stepSolver problem
    let penalties = Penalty.figurePenalties problem
    fun (figure, figurePenalty) ->
        if figurePenalty = 0.0 then
            printfn "Reached score 0 and will not advance"
            (figure, figurePenalty)
        else
            let resultOpt, penalty = stepSolver figure
            let result = Option.defaultValue figure resultOpt
            let resPenalties = penalties result
            let spenalties =
                resPenalties
                |> List.map (fun p -> sprintf "%.2f" p) 
                |> String.concat " + "
            printfn $"penalty = {spenalties} = {List.sum(resPenalties)}"
            (result, penalty)

module MVU =
    open Elmish
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls.Shapes

    type Tool = Move | Rotate

    type State = {
        Problem: Model.Problem
        History: ResizeArray<Model.Figure * float>
        Index: int // position in History
        Scale: float
        Origo: Model.Coord
        Selection: int list
        Tool: Tool
        InProgress: ((int * int) * (int * int)) option
    }

    type Msg =
        | Id | Forward of int | Backward of int | Reset | Save | ZoomIn | ZoomOut | SelectTool of Tool
        | Select of Avalonia.Point | SelectAll | DeselectAll
        | CanvasPressed of Avalonia.Point
        | CanvasReleased of Avalonia.Point
        | CanvasMoved of Avalonia.Point

    let init (problem: Model.Problem): State * Cmd<Msg> =
        let maxCoord = Array.maxBy (fun (c: Model.Coord) -> max c.X c.Y) problem.Figure.Vertices
        {
            Problem = problem
            History = ResizeArray([problem.Figure, infinity])
            Index = 0
            Scale = 0.9 * WINDOWSIZE / float (max maxCoord.X maxCoord.Y)
            Selection = []
            Tool = Move
            InProgress = None
            Origo = Model.Coord (0, 0)
        },
        Cmd.OfFunc.attempt (fun problem ->
                stepperGlobalVar := Some (stepSolverWithStopAndDebug problem)
            ) problem (fun _ -> Id)

    // adds a new figure based on the current figure, if the current figure
    // is the last figure in the history
    let applyIfLast (f: Model.Figure -> Model.Figure) (state: State): State =
        if state.Index = state.History.Count - 1 then
            let newFig = f (fst state.History.[state.Index])
            state.History.Add (newFig, infinity)
            { state with Index = state.Index + 1 }
        else state

    let update (msg: Msg) (state: State): (State * Cmd<Msg>) =
        let pointToCoord (p: Avalonia.Point) = Model.Coord (int(p.X / state.Scale), int(p.Y / state.Scale))
        match msg with
        | Id -> state, Cmd.none
        | Forward steps ->
            let (_, curScore) = state.History.[state.History.Count - 1]
            let newIndex = state.Index + steps
            let stepper = Option.get !stepperGlobalVar
            while newIndex >= state.History.Count do
                let lastState = state.History.[state.History.Count - 1]
                let rec repeatFn i f x =
                    match i with
                    | 0 -> x
                    | n -> repeatFn (n - 1) f (f x)
                state.History.Add (repeatFn 10 stepper lastState)
            { state with Index = newIndex }, Cmd.none
        | Backward steps -> { state with Index = max 0 (state.Index - steps) }, Cmd.none
        | Reset -> init state.Problem
        | Save ->
            state,
            Cmd.OfFunc.attempt
                (fun state ->
                    let figure = fst state.History.[state.Index]
                    let writeSolution = Option.get !writeSolutionGlobalVar
                    writeSolution figure)
                state
                (fun _ -> Id)
        | ZoomIn -> { state with Scale = state.Scale * 1.50 }, Cmd.none
        | ZoomOut -> { state with Scale = state.Scale / 1.50 }, Cmd.none
        | Select p ->
            let selectedCoordIndex = findNearbyCoord (pointToCoord p) (fst state.History.[state.Index])
            match selectedCoordIndex with
            | None -> state, Cmd.none
            | Some index ->
                if List.contains index state.Selection then
                    { state with Selection = List.filter (fun i -> i <> index) state.Selection }, Cmd.none
                else
                    { state with Selection = (index::state.Selection) |> Seq.distinct |> List.ofSeq }, Cmd.none
        | SelectAll ->
            { state with Selection = Array.mapi (fun i _ -> i) (fst state.History.[state.Index]).Vertices |> List.ofSeq }, Cmd.none
        | DeselectAll ->
            { state with Selection = [] }, Cmd.none
        | CanvasPressed p ->
            let x, y = int(p.X / state.Scale), int(p.Y / state.Scale)
            { state with InProgress = Some ((x, y), (x, y)) }, Cmd.none
        | CanvasMoved p ->
            match state.InProgress with
            | Some ((x1, y1), _) ->
                let x2, y2 = int(p.X / state.Scale), int(p.Y / state.Scale)
                { state with InProgress = Some ((x1, y1), (x2, y2)) }, Cmd.none
            | _ -> state, Cmd.none
        | CanvasReleased p ->
            let x2, y2 = int(p.X / state.Scale), int(p.Y / state.Scale)
            match state.Tool, state.InProgress with
            | Move, Some ((x1, y1), _) ->
                let dx, dy = x2 - x1, y2 - y1
                let translatedState = applyIfLast (Transformations.translateSelectedVerticies state.Selection (dx, dy)) state
                { translatedState with InProgress = None }, Cmd.none
            | Rotate, Some ((x1, y1), _) ->
                if x1 = x2 && y1 = y2 then
                    { state with Origo = pointToCoord p; InProgress = None }, Cmd.none
                else
                    let dy = y2 - y1
                    let translatedState = applyIfLast (Transformations.rotateSelectedVerticiesAroundByAngle state.Selection state.Origo (float dy)) state
                    { translatedState with InProgress = None }, Cmd.none
            | _ -> state, Cmd.none
        | SelectTool tool -> { state with Tool = tool; InProgress = None }, Cmd.none
    
    let view (state: State) (dispatch) =
        let scale = state.Scale
        let (figure, penalty) = state.History.[state.Index]
        let shownFigure =
            match state.Tool, state.InProgress with
            | Move, Some ((x1, y1), (x2, y2)) ->
                let (dx, dy) = (x2 - x1, y2 - y1)
                Transformations.translateSelectedVerticies state.Selection (dx, dy) figure
            | Rotate, Some ((x1, y1), (x2, y2)) ->
                let dy = y2 - y1
                Transformations.rotateSelectedVerticiesAroundByAngle state.Selection state.Origo (float dy) figure
            | _ -> figure
        let vs = shownFigure.Vertices
        let holeSegments = Model.holeSegments state.Problem
        let segmentOutsideHole = Penalty.segmentOutsideHole holeSegments
        let adj, isArticulationPoint = Graph.getArticulationPoints shownFigure
        let verticalCutLines = Graph.findVerticalCutComponents adj shownFigure
        let horizontalCutLines = Graph.findHorizontalCutComponents adj shownFigure
        DockPanel.create [
            DockPanel.children [
                UniformGrid.create [
                    UniformGrid.dock Dock.Bottom
                    UniformGrid.columns 6
                    UniformGrid.children [
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Backward 100))
                            Button.content "<<<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Backward 10))
                            Button.content "<<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Backward 1))
                            Button.content "<"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Forward 1))
                            Button.content ">"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Forward 10))
                            Button.content ">>"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch (Forward 100))
                            Button.content ">>>"
                        ]
                    ]
                ]
                UniformGrid.create [
                    UniformGrid.dock Dock.Bottom
                    UniformGrid.columns 2
                    UniformGrid.children [
                        Button.create [
                            Button.onClick (fun _ -> dispatch ZoomOut)
                            Button.content "Zoom out"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch ZoomIn)
                            Button.content "Zoom in"
                        ]
                        Button.create [
                            Button.dock Dock.Bottom
                            Button.onClick (fun _ -> dispatch Reset)
                            Button.content "Reset"
                        ]
                        Button.create [
                            Button.dock Dock.Bottom
                            Button.onClick (fun _ -> dispatch Save)
                            Button.content "Save"
                        ]
                    ]
                ]
                UniformGrid.create [
                    UniformGrid.dock Dock.Bottom
                    UniformGrid.columns 4
                    UniformGrid.children [
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
                        Button.create [
                            Button.content "Select all"
                            Button.onClick (fun _ -> dispatch SelectAll)
                        ]
                        Button.create [
                            Button.content "Deselect all"
                            Button.onClick (fun _ -> dispatch DeselectAll)
                        ]
                    ]
                ]
                TextBox.create [
                    TextBox.dock Dock.Bottom
                    TextBox.text (sprintf $"Step: {state.Index}, Cost: {penalty}")
                ]
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.onPointerPressed (fun evt ->
                        if evt.Route = Avalonia.Interactivity.RoutingStrategies.Tunnel then
                            let shift = evt.KeyModifiers.HasFlag (Avalonia.Input.KeyModifiers.Shift)
                            let position = evt.GetPosition null
                            match shift with
                            | true -> dispatch (Select position)
                            | _ -> dispatch (CanvasPressed position))
                    Canvas.onPointerMoved (fun evt ->
                            dispatch (CanvasMoved (evt.GetPosition null)))
                    Canvas.onPointerReleased (fun evt ->
                        if evt.Route = Avalonia.Interactivity.RoutingStrategies.Tunnel then
                            dispatch (CanvasReleased (evt.GetPosition null)))
                    Canvas.children (
                        (
                            figure.Edges
                            |> Array.toList
                            |> List.map (fun (s,t) ->
                                let sc, tc = vs.[s], vs.[t]
                                let outsideHolePenalty = segmentOutsideHole (sc, tc)
                                let color =
                                    if outsideHolePenalty > Geometry.EPSILON then "#00FFFF"
                                    else if outsideHolePenalty < -Geometry.EPSILON then "#E74C3C"
                                    else "#00FF00"
                                Line.create [
                                    Line.startPoint (float sc.X * scale, float sc.Y * scale)
                                    Line.endPoint (float tc.X * scale, float tc.Y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke color
                                ] :> Avalonia.FuncUI.Types.IView
                            )
                        ) @
                        (
                            figure.Vertices
                            |> Array.mapi (fun i c -> (i, c))
                            |> Array.filter (fun (i, _) -> isArticulationPoint.[i])
                            |> Array.toList
                            |> List.map (fun (_, c) ->
                                Ellipse.create [
                                    Ellipse.left (float(c.X) * scale - (0.75 * scale))
                                    Ellipse.top (float(c.Y) * scale - (0.75 * scale))
                                    Ellipse.width (1.5 * scale)
                                    Ellipse.height (1.5 * scale)
                                    Ellipse.fill "#ADD8E6"
                                ] :> Avalonia.FuncUI.Types.IView)
                        ) @
                        (
                            state.Selection
                            |> List.map (fun i ->
                                let c = vs.[i]
                                Ellipse.create [
                                    Ellipse.left (float(c.X) * scale - (1.5 * scale))
                                    Ellipse.top (float(c.Y) * scale - (1.5 * scale))
                                    Ellipse.width (3.0 * scale)
                                    Ellipse.height (3.0 * scale)
                                    Ellipse.fill "#95a5a6"
                                ] :> Avalonia.FuncUI.Types.IView)
                        )  @
                        (
                            verticalCutLines
                            |> Seq.map (fun (x, _) ->
                                Line.create [
                                    Line.startPoint (float x * scale, 0.0)
                                    Line.endPoint (float x * scale, 2000.0)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#D3D3D3"
                                ] :> Avalonia.FuncUI.Types.IView)
                            |> List.ofSeq
                        ) @
                        (
                            horizontalCutLines
                            |> Seq.map (fun (y, _) ->
                                Line.create [
                                    Line.startPoint (0.0, float y * scale)
                                    Line.endPoint (2000.0, float y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#D3D3D3"
                                ] :> Avalonia.FuncUI.Types.IView)
                            |> List.ofSeq
                        ) @
                        (
                            [
                                Line.create [
                                    Line.startPoint (float state.Origo.X * scale, 0.0)
                                    Line.endPoint (float state.Origo.X * scale, 2000.0)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#D3D3D3"
                                ] :> Avalonia.FuncUI.Types.IView;
                                Line.create [
                                    Line.startPoint (0.0, float state.Origo.Y * scale)
                                    Line.endPoint (2000.0, float state.Origo.Y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#D3D3D3"
                                ] :> Avalonia.FuncUI.Types.IView
                            ]
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
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "TestUI"
        base.Width <- WINDOWSIZE
        base.Height <- WINDOWSIZE
        
        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let problem = Option.get !problemGlobalVar

        Program.mkProgram MVU.init MVU.update MVU.view
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

let showGui (problem: Model.Problem) (writeSolution: Model.Figure -> unit) =
        problemGlobalVar := Some problem 
        writeSolutionGlobalVar := Some writeSolution
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime([||]) // TODO: how to parse args
