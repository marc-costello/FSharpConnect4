namespace Connect4

module Game =

    open Types

    let startGame() = { Board = (Grid.blankGrid 6 7); Player = Player.Red }

    let setActivePlayer game player = { Board=game.Board; Player = player }

    let gameLoop =
        