namespace Connect4

module Game =

    open Types

    let createNewGame = fun () -> { Board = (Grid.blankGrid 6 7); ActivePlayer = Player.Red; MoveEvent = new Event<MoveEvent>() }

    let moveHandler (move:MoveEvent) = 
        //System.Console.ReadKey |> 
        printfn "Move event triggered : %A" move.Player

    let setActivePlayer game player = { Board=game.Board; ActivePlayer=player; MoveEvent= new Event<MoveEvent>() }

    let beginGame (game:Game) =
        let publiblishedEvent = game.MoveEvent.Publish
        publiblishedEvent.Add moveHandler