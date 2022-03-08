module App

open Elmish
open Feliz
open Feliz.UseElmish
open Fable.Core.JS

type Player =
  | X
  | O

type Cell = int
type Turn = Cell * Player
type Board = Cell list

type GameFinished = { winner: Player }
type GameOngoing = { player: Player; turns: Turn list }

type GameState =
  | GameFinished
  | GameOngoing

type Msg = Move of Turn

type State = Gamestate

let init () = { player = X; turns = [] }, Cmd.none

let deriveNextPlayer (player: Player) =
  match player with
  | X -> O
  | _ -> X

let renderCell (c: Cell, onClick) =
  Html.div [
    prop.className "cell"
    prop.onClick onClick
    prop.text c
  ]

let renderCells (cells, onClick) =
  Html.div [
    prop.className "board"
    prop.children [
      yield!
        cells
        |> Seq.map (fun x -> renderCell (x, (fun _ -> onClick (x))))
    ]
  ]

let makeCells = [ 0..8 ] |> List.map (fun x -> x)

let derivePlayerLabel player =
  match player with
  | X -> Html.text "X"
  | O -> Html.text "O"

let update msg state =
  match msg with
  | Move turn ->
    { state with
        player = deriveNextPlayer state.player
        turns = state.turns |> List.append [ turn ] },
    Cmd.none

[<ReactComponent>]
let Game () =
  let state, dispatch = React.useElmish (init, update, [||])

  let onClick (cell: Cell) = dispatch (Move(cell, state.player))

  Html.div [
    prop.children [
      Html.h1 [
        derivePlayerLabel (state.player)
      ]
      Html.div [
        prop.children [
          renderCells (makeCells, onClick)
        ]
      ]
    ]
  ]
