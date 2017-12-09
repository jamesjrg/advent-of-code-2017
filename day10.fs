module Advent.Day10

open System

let tests = [
    ("", 1)
]

let doProcess (text:string) =
    0

let day10 () =
    let puzzleInput = System.IO.File.ReadAllText("day10input.txt");

    for test in tests do
        let score = doProcess (fst test)
        printfn "expected %d actual %d" score (snd test)
