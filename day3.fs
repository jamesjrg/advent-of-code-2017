module Advent.Day3

let test1 = 1 // 0
let test2 = 12  // 3
let test3 = 23 // 2
let test4 = 1024 // 31
let puzzleInput = 312051

type StepType =
    | Horizontal1 = 0
    | Vertical1 = 1
    | Horizontal2 = 2
    | Vertical2 = 3

let day3Calculator (input:int) = 
    let mutable x = 0
    let mutable y = 0
    let mutable stepType = StepType.Horizontal1
    let mutable movesToTarget = 1
    let mutable nextTarget = 1

    for i in 2..input do
        match stepType with
        | StepType.Horizontal1 ->
            x <- x + 1
        | StepType.Vertical1 ->
            y <- y + 1
        | StepType.Horizontal2 ->
            x <- x - 1
        | StepType.Vertical2 ->
            y <- y - 1

        movesToTarget <- movesToTarget - 1

        if movesToTarget = 0 then
            if stepType = StepType.Vertical1 || stepType = StepType.Vertical2 then nextTarget <- nextTarget + 1
            movesToTarget <- nextTarget

            if stepType = StepType.Vertical2 then stepType <- StepType.Horizontal1 else stepType <- enum ((int stepType) + 1)

    abs x + abs y    
    

let day3 =
    printfn "%d" (day3Calculator test1)
    printfn "%d" (day3Calculator test2)
    printfn "%d" (day3Calculator test3)
    printfn "%d" (day3Calculator test4)
    printfn "%d" (day3Calculator puzzleInput)