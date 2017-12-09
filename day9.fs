module Advent.Day9

let tests = [
    ("{}", 1);
    ("{{{}}}", 6);
    ("{{},{}}", 5);
    ("{{{},{},{{}}}}", 16);
    ("{<a>,<a>,<a>,<a>}", 1);
    ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9);
    ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
    ]

let rec skipGarbage (text:string) pointer garbageCount = 
    match text.[pointer] with
    | '>' -> (pointer + 1), garbageCount
    | '!' -> skipGarbage text (pointer + 2) garbageCount
    | _ -> skipGarbage text (pointer + 1) (garbageCount + 1)

    
let scoreGroups (text:string) = 

    let rec next pointer depth score garbageCount =
        if pointer = text.Length then
            score, garbageCount
        else
            let pointer, garbageCount, depth, score =
                match text.[pointer] with
                | '{' -> pointer + 1, garbageCount, depth + 1, score
                | '}' -> pointer + 1, garbageCount, depth - 1, score + depth
                | '<' ->
                    let x, y = skipGarbage text (pointer + 1) garbageCount
                    x, y, depth, score
                | ',' -> (pointer + 1), garbageCount, depth, score
                | _ -> failwith "wtf"
            next pointer depth score garbageCount

    next 0 0 0 0

let day9 () =
    let puzzleInput = System.IO.File.ReadAllText("day9input.txt");

    for test in tests do
        let score, _ = scoreGroups (fst test)
        printfn "expected %d actual %d" (snd test) score

    printfn "%A" (scoreGroups puzzleInput)
