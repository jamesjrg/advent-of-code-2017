module Advent.Day10

open System
open FParsec.Error

let puzzleInput = ("130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224", 256)

let test1 = ("3, 4, 1, 5", 5, 12)

let tests = [
    ("", 1)
]

let tieKnots (input:string) (length:int) =

    let rec tie remaining circle pointer =
        match remaining with
        | [] -> circle
        | h :: t -> tie t circle pointer

    let input = input.Split(',') |> Seq.map int |> List.ofSeq
    let initial = [0..length]
    let result = tie input initial 0

    result.[0] * result.[1]

let day10 () =
    let input, length, expected = test1
    let actual = tieKnots input length
    printfn "expected %d actual %d" expected actual
