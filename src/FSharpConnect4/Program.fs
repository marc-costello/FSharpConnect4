open Connect4
open Connect4.Types
open Connect4.Grid

open Game

[<EntryPoint>]
let main argv =
    beginGame <| Game.createNewGame()
    0 // return an integer exit code