module GameTests

open NUnit.Framework
open FsUnit

open Connect4
open Connect4.Types

open Connect4.Game

[<Test>]
let ``human player should start the game``() =
    let game = Game.startGame()
    game.Player |> should equal <| Red