module Advent.Day4

let partOneTests = [
    ("aa bb cc dd ee", true)
    ("aa bb cc dd aa" , false)
    ("aa bb cc dd aaa", true)
]

let partTwoTests = [
    ("abcde fghij", true)
    ("abcde xyz ecdab" , false)
    ("a ab abc abd abf abj", true)
    ("iiii oiii ooii oooi oooo", true)
    ("oiii ioii iioi iiio", false)
]

let checkValidPartOne (passphrase:string) = 
    let words = passphrase.Split()
    words |> Seq.countBy id |> Seq.exists (fun (k, count) -> count > 1)

let checkValidPartTwo (passphrase:string) = 
    passphrase.Split()
        |> Seq.map ((Seq.sort) >> (fun x -> System.String.Concat(x)))
        |> Seq.groupBy id
        |> Seq.where (fun (k, v) -> Seq.length v > 1)
        |> Seq.isEmpty


let countValidPartOne (text:string) = 
    let rows = text.Split('\n')
    rows |> Seq.where checkValidPartOne |> Seq.length    

let countValidPartTwo (text:string) = 
    let rows = text.Split('\n')
    rows |> Seq.where checkValidPartTwo |> Seq.length    

let day4 () =
    let puzzleInput = System.IO.File.ReadAllText("day4input.txt");

    for input, expected in partTwoTests do
        printfn "expected: %b actual: %b" expected (checkValidPartTwo input)

    printfn "%d" (countValidPartTwo puzzleInput)    