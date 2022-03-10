module Types

type Player =
  | X
  | O

type CellState =
  | Empty
  | Player of Player

type Cell = { state: CellState; id: int }
type Turn = Cell * Player
type Board = Cell list

type GameState =
  { player: Player
    turns: Turn list
    winner: option<Player> }

type Msg = Move of Turn
type State = Gamestate
