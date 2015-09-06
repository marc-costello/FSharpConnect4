namespace Connect4

module GameConstants =
    
    [<Literal>]
    let COLUMNS = 7

    [<Literal>]
    let ROWS = 6


module Types =

    type Player =
        | Red
        | Yellow
    type Coin =
        | Player of Player
        | Empty

    type GridCoordinate = int * int
    type Grid = Coin[,]
    type Move = GridCoordinate * Coin

    type GameState =
        | Won
        | Draw
        | InProgress

    type Direction = 
        | PlusLeft 
        | PlusRight 
        | NegativeLeft 
        | NegativeRight

    type Game = { Board:Grid; Player:Player }

    


module Grid =

    open Types
    
    module Array2D =
        let flatten (arr:'T[,]) : 'T[] = 
            let xMaxIndex = (Array2D.length1 arr) - 1
            seq { for i in 0..xMaxIndex do yield arr.[i,*] } 
            |> Array.concat

    let blankGrid x y : Grid = Array2D.create x y Empty

    // todo DO I NEED THIS NOW??
//    let canMakeMoveOnGrid (move:Move) (grid:Grid) =
//        let (moveCoord, coin) = move
//        let (x, y) = moveCoord
//        let existingCoin = 
//            try 
//                Some grid.[x, y]
//            with
//            | _ -> None
//
//        match existingCoin with
//        | Some Empty -> true
//        | _ -> false

    let makeMove grid move : Grid =
        let (moveCoord, coin) = move
        let copy = Array2D.copy grid
        copy.[(fst moveCoord),(snd moveCoord)] <- coin
        copy
    
    let dropCoin (grid:Grid) player columnIndex =
        let rowIndex = grid.[columnIndex, *] |> Array.tryFindIndex(fun x -> x = Empty)
        match rowIndex with
        | Some i -> (true, makeMove grid ((columnIndex, i), player))
        | None -> (false, grid)

    let checkGridGroupForWinner (player:Player) column =
        let count = 
            List.fold (fun acc coin ->
                match coin, acc with
                | Player(player), _ -> acc + 1
                | _, 4 -> 4
                | _, _ -> 0
            ) 0 column
        count >= 4

    let checkForHorizontalWin player (grid:Coin[,]) =
        let mutable result = []
        for i in 0..(Array2D.length1 grid - 1) do
            let gr = 
                grid.[i,*]
                |> Array.toList
                |> checkGridGroupForWinner player
            result <- gr::result
        result |> List.exists (fun x -> x = true)


    let checkForVerticalWin player (grid:Coin[,]) = 
        let mutable result = []
        for i in 0..(Array2D.length2 grid - 1) do
            let gr = 
                grid.[*,i]
                |> Array.toList
                |> checkGridGroupForWinner player
            result <- gr::result
        result |> List.exists (fun x -> x = true)

    let checkForDiagonalWin player (flatGrid:Coin[]) (anchor:int * int) = 
        let startIndex = (fst anchor * GameConstants.COLUMNS) + snd anchor
        let maxGridIndex = flatGrid.Length - 1
        
        let indexes startingPos increment = 
            let rec loop accindex =
                match accindex with
                | i when i < 0 || i > maxGridIndex -> []
                | i -> i :: loop (i + increment)
            loop startingPos
        
        let convertIndexesIntoGridEntries indexes =
            indexes |> List.map (fun i -> flatGrid.[i])
        
        [6;8;-6;-8] 
        |> List.map (indexes startIndex)
        |> List.map (fun l -> convertIndexesIntoGridEntries l)
        |> List.exists (fun l -> checkGridGroupForWinner player l)
        
    let checkGridForDraw flatGrid =
        flatGrid |> Array.exists (fun x -> x = Empty) |> not

    // todo
    let render (grid:Grid) =
        Array2D.iteri (fun i y item ->
            if i > GameConstants.COLUMNS then (printf "%A" item) else (printfn "%A" item)
        )