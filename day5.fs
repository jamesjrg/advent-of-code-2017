module Advent.Day5

open System

let test1 = "0
3
0
1
-3"

let day5Calculator (text:string) = 
    let jumpList = text.Split('\n') |> Array.map int
    
    let rec jump pointer count =
        if pointer < 0 || pointer >= jumpList.Length then
            count
        else
            let oldJump = jumpList.[pointer]
            jumpList.[pointer] <- oldJump + 1
            jump (pointer + oldJump) (count + 1)

    jump 0 0
    

let day5 () =
    let puzzleInput = System.IO.File.ReadAllText("day5input.txt");
    
    printfn "%d" (day5Calculator test1)
    printfn "%d" (day5Calculator puzzleInput)