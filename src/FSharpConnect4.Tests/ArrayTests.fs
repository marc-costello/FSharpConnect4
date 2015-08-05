module ArrayTests

open NUnit.Framework
open FsUnit

open Connect4
open Connect4.Grid


[<Test>]
let ``flatten a 2D array``() =
    let array = Array2D.create 7 6 0 |> Array2D.mapi (fun _ y _ -> y)
    let flattenedArray = Array2D.flatten array
    let result = seq { for i in 1..7 do yield [|0..5|] } |> Array.concat
    flattenedArray |> should equal result