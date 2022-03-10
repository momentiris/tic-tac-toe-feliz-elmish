module PlayerLabel

open Types
open Feliz

[<ReactComponent>]
let PlayerLabel (player: Player) =
  match player with
  | X -> Html.div [ prop.text "X" ]
  | O -> Html.div [ prop.text "O" ]
