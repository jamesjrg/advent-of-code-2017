module Advent.Day4

let test1 = "aa bb cc dd ee" // is valid
let test2 = "aa bb cc dd aa" // is not valid - the word aa appears more than once.
let test3 = "aa bb cc dd aaa" // is valid

let checkValid (passphrase:string) = 
    let words = passphrase.Split()
    words |> Seq.groupBy id |> Seq.where (fun (k, v) -> Seq.length v > 1) |> Seq.isEmpty

let day4Calculator (text:string) = 
    let rows = text.Split('\n')
    rows |> Seq.where checkValid |> Seq.length
    

let day4 () =
    let puzzleInput = System.IO.File.ReadAllText("day4input.txt");
    
    printfn "%b" (checkValid test1)
    printfn "%b" (checkValid test2)
    printfn "%b" (checkValid test3)
    printfn "%d" (day4Calculator puzzleInput)