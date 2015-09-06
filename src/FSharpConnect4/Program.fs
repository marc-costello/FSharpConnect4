open Connect4
open Connect4.Types
open Connect4.Grid

open Game

[<EntryPoint>]
let main argv =
    let game =  Game.startGame()
    0 // return an integer exit code