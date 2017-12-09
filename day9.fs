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

let rec skipGarbage (text:string) pointer = 
    match text.[pointer] with
    | '>' -> pointer + 1
    | '!' -> skipGarbage text (pointer + 2)
    | _ -> skipGarbage text (pointer + 1)

    
let scoreGroups (text:string) = 

    let rec next pointer depth score =
        if pointer = text.Length then
            score
        else
            let newPointer, depth, score =
                match text.[pointer] with
                | '{' -> pointer + 1, depth + 1, score
                | '}' -> pointer + 1, depth - 1, score + depth
                | '<' -> (skipGarbage text (pointer + 1)), depth, score
                | ',' -> (pointer + 1), depth, score
                | _ -> failwith "wtf"
            next newPointer depth score


    next 0 0 0

let day9 () =
    let puzzleInput = System.IO.File.ReadAllText("day9input.txt");

    for test in tests do
        let score = scoreGroups (fst test)
        printfn "expected %d actual %d" (snd test) score

    printfn "%d" (scoreGroups puzzleInput)
