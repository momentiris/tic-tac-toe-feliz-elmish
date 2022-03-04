module App

open Elmish
open Feliz
open Feliz.UseElmish
open Fable.Core.JS

type Player =
    | X
    | O

type CellState =
    | Empty
    | Player of Player

type Cell = { id: int; cellState: CellState }
type Turn = { cell: Cell; player: Player }
type Board = Cell list
type GameFinished = { winner: Player }
type GameOngoing = { turn: Player }

type GameState =
    | GameFinished
    | GameOngoing

type Msg =
    | Continue
    | Winner

type State = Gamestate

let init () = { turn = X }, Cmd.none

let deriveNextTurn (player: Player) =
    match player with
    | X -> O
    | _ -> X

let update msg state =
    match msg with
    | Continue -> { state with turn = deriveNextTurn state.turn }, Cmd.none
    | Winner -> { state with turn = deriveNextTurn state.turn }, Cmd.none

let renderCell (c: Cell, onClick) =
    Html.div [ prop.className "cell"
               prop.onClick onClick
               prop.text c.id ]

let renderCells (cells: Cell list, onClick) =
    Html.div [ prop.className "board"
               prop.children [ yield!
                                   cells
                                   |> Seq.map (fun x -> renderCell (x, (fun _ -> onClick (x.id)))) ] ]

let makeCell (id: int) = { id = id; cellState = Empty }

let makeCells = [ 0..8 ] |> List.map makeCell

[<ReactComponent>]
let Game () =
    let state, dispatch = React.useElmish (init, update, [||])

    let onClick (id: int) = dispatch (Continue)

    Html.div [ Html.div [ prop.children [ renderCells (makeCells, onClick) ] ] ]

//    Html.button [ prop.text "Increment"
//                  prop.onClick (fun _ -> dispatch Increment) ]

//    Html.button [ prop.text "Decrement"
//                  prop.onClick (fun _ -> dispatch Decrement) ] ]                ]
