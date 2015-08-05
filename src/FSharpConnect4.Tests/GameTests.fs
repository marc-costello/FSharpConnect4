module GameTests

open NUnit.Framework
open FsUnit

open Connect4
open Connect4.Types

open Connect4.Game

let private createGame = fun () -> Game.createNewGame()

[<Test>]
let ``human player should start the game``() =
    let game = createGame()
    beginGame game
    game.ActivePlayer |> should equal Human

 