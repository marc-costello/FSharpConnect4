module GridTests

open NUnit.Framework
open FsUnit

open Connect4
open Connect4.Types

open Grid


let private blankConnect4TestGrid() = Grid.blankGrid 6 7

let private fillColumn columnIndex (gridToFill:Grid) =
    gridToFill.[columnIndex, *] 
    |> Array.iteri (fun i _ -> gridToFill.[columnIndex, i] <- Player(Red))
    gridToFill

[<Test>]
let ``blankgrid returns a blank connect4 grid full of empties``() =
    let blankGrid = Grid.blankGrid 6 7
    Array2D.length1 blankGrid |> should equal 6
    Array2D.length2 blankGrid |> should equal 7

[<Test>]
let ``a player can make a move on the grid``() =
    let blankGrid = blankConnect4TestGrid()
    let move = ((0,0), Player(Red))
    let playedGrid = Grid.makeMove blankGrid move
    playedGrid.[0,0] |> should be (sameAs <| snd move)

[<Test>]
let ``dropping a coin on a column finds the first available empty space``() =
    let grid1 = blankConnect4TestGrid()
    grid1.[1,0] <- Player(Yellow)
    grid1.[1,1] <- Player(Red)
    let (_, gridAfterDrop) = Grid.dropCoin grid1 (Player(Red)) 1
    gridAfterDrop.[1,2] |> should equal <| Player(Red)

[<Test>]
let ``dropping a coin successfully returns true and the grid``() =
    let grid2 = blankConnect4TestGrid()
    let (result, newGrid) = Grid.dropCoin grid2 (Player(Red)) 0
    result |> should equal true
    newGrid.[0,0] |> should equal <| Player(Red) 

[<Test>]
let ``dropping a coin on a full column returns false and the grid``() =
    let grid3 = blankConnect4TestGrid() |> fillColumn 0
    let (result, _) = Grid.dropCoin grid3 (Player(Red)) 0
    result |> should equal false

[<Test>]
let ``making a move returns a new grid, immutable``() =
    let blankGrid = blankConnect4TestGrid()
    let move1 = ((0,0), Player(Red))
    let move2 = ((0,1), Player(Yellow))
    let gridWithFirstMove = Grid.makeMove blankGrid move1
    let gridWithSecondMove = Grid.makeMove blankGrid move2
    gridWithFirstMove |> should not' (be sameAs gridWithSecondMove)

[<Test>]
let ``if we cant drop a coin on the grid, return the same grid``() =
    let grid = blankConnect4TestGrid() |> fillColumn 0
    let (_, gridAfterMove) = Grid.dropCoin grid (Player(Red)) 0
    grid |> should be (sameAs gridAfterMove)

[<Test>]
let ``given a single sequence of coins detect connect4``() =
    let column = [for i in 1..7 do yield if i <= 4 then Player(Red) else Empty]
    let winner = Grid.checkGridGroupForWinner Red column
    winner |> should equal true

[<Test>]
let ``detect a draw``() =
    let fullGrid = 
        Array2D.mapi (fun x y _ -> if (x + y) % 2 = 0 then Player(Red) else Player(Yellow)) <| blankConnect4TestGrid()
    let draw = Grid.checkGridForDraw <| Array2D.flatten fullGrid
    draw |> should equal true

[<Test>]
let ``detect connect4 horizontally for a given player``() =
    let blankGrid = blankConnect4TestGrid()
    let winningGrid = [0..3] |> List.fold (fun accGrid i -> Grid.makeMove accGrid ((0,i), Player(Red))) blankGrid
    let win = Grid.checkForHorizontalWin Red winningGrid
    win |> should equal true

[<Test>]
let ``detect connect4 vertically for a given player``() =
    let blankGrid = blankConnect4TestGrid()
    let winningGrid = [0..3] |> List.fold (fun accGrid i -> Grid.makeMove accGrid ((i,0), Player(Red))) blankGrid
    let win = Grid.checkForVerticalWin Red winningGrid
    win |> should equal true

[<Test>]
let ``detect connect4 diagonally for a given move``() =
    let blankGrid = blankConnect4TestGrid()
    let winningGrid = [0..3] |> List.fold (fun accGrid i -> Grid.makeMove accGrid ((i,i), Player(Red))) blankGrid

    let win1 = Grid.checkForDiagonalWin Red <| Array2D.flatten winningGrid <| (0, 0)
    let win2 = Grid.checkForDiagonalWin Red <| Array2D.flatten winningGrid <| (3, 3)
    
    win1 |> should equal true
    win2 |> should equal true