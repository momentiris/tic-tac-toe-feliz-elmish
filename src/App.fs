module App

open Elmish
open Feliz
open Feliz.UseElmish
open Fable.Core.JS

open Types
open PlayerLabel

let init () =
  { player = X
    turns = []
    winner = None },
  Cmd.none

let deriveNextPlayer (player) =
  match player with
  | X -> O
  | _ -> X

let derivePlayerLabelTextFromCellState (cellState) =
  match cellState with
  | Empty -> ""
  | Player (p) ->
    match p with
    | X -> "X"
    | O -> "O"

let renderCell (cellState, onClick) =
  Html.div [
    prop.className "cell"
    prop.onClick onClick
    prop.children [
      Html.text (derivePlayerLabelTextFromCellState (cellState))
    ]
  ]

let renderCells (cells, onClick) =
  Html.div [
    prop.className "board"
    prop.children [
      yield!
        cells
        |> Seq.map (fun x -> renderCell (x.state, (fun _ -> onClick (x))))
    ]
  ]

let deriveCellStateFromTurns (id: int, turns: Turn list) : CellState =
  match turns |> List.tryFind (fun (c, _) -> c.id = id) with
  | None -> Empty
  | Some turn -> Player(snd turn)

let makeCells (cells: int List, turns: Turn list) =
  cells
  |> List.map (fun id ->
    { state = deriveCellStateFromTurns (id, turns)
      id = id })

let derivePlayerLabel player =
  match player with
  | X -> Html.text "X"
  | O -> Html.text "O"

let isAllowedTurn (turns: Turn list) (turn: Turn) =
  not (
    turns
    |> List.exists (fun t -> (fst t).id = (fst turn).id)
  )

let update msg state =
  match msg with
  | Move turn ->
    match isAllowedTurn state.turns turn with
    | false -> state, Cmd.none
    | true ->
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
      PlayerLabel state.player
      Html.div [
        prop.children [
          renderCells (makeCells ([ 0..8 ], state.turns), onClick)
        ]
      ]
    ]
  ]
