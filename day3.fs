module Advent.Day3

open System.Collections.Generic

let test1 = 1 // 0
let test2 = 12  // 3
let test3 = 23 // 2
let test4 = 1024 // 31
let puzzleInput = 312051

type Step = { sequence: int; xDiff: int; yDiff: int }

let right = {sequence = 0; xDiff = 1; yDiff = 0}
let up = {sequence = 1; xDiff = 0; yDiff = 1}
let left = {sequence = 2; xDiff = -1; yDiff = 0}
let down = {sequence = 3; xDiff = 0; yDiff = -1}

let steps = [| right; up; left; down |]

let sumNeighbours (x:int) (y:int) (valueLookup:Dictionary<int * int, int>) =
    [(-1, -1); (-1,0); (-1,1); (0,1); (1,1); (1, 0); (1, -1); (0, -1)]
    |> Seq.fold (fun acc (xOffset, yOffset) ->
        let _, value = valueLookup.TryGetValue ((x + xOffset, y + yOffset))
        acc + value) 0

let day3Calculator (input:int) = 
    let mutable x = 0
    let mutable y = 0
    let mutable stepIndex = 0
    let mutable movesToTarget = 1
    let mutable nextTarget = 1
    let valueLookup = Dictionary<int * int, int>()
    valueLookup.[(0, 0)] <- 1
    let mutable firstValueLargerThanInput = Option<int>.None

    for i in 2..input do
        let step = steps.[stepIndex]
        x <- x + step.xDiff
        y <- y + step.yDiff

        let sum = sumNeighbours x y valueLookup
        valueLookup.[(x, y)] <- sum

        match sum, firstValueLargerThanInput with
        | sum, None when sum > input -> firstValueLargerThanInput <- Some sum
        | _ -> ()

        movesToTarget <- movesToTarget - 1

        if movesToTarget = 0 then
            if step = up || step = down then nextTarget <- nextTarget + 1
            movesToTarget <- nextTarget
            if step = down then stepIndex <- 0 else stepIndex <- step.sequence + 1

    abs x + abs y, firstValueLargerThanInput
    

let day3 () =
    for input in [test1;test2;test3;test4;puzzleInput] do
        let manhatten, firstLarger = day3Calculator input
        printfn "%d %d" manhatten (if firstLarger.IsSome then firstLarger.Value else 0)