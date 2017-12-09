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

let scoreGroups (text:string) = 
    let rows = text.Split('\n')
    0

let day9 () =
    let puzzleInput = System.IO.File.ReadAllText("day9input.txt");
    
    for test in tests do
        let score = scoreGroups (fst test)
        printfn "expected %d actual %d" score (snd test)