module Main

open Browser
open Feliz

Fable.Core.JsInterop.importSideEffects "./App.css"


ReactDOM.render (App.Game(), document.getElementById "feliz-app")
