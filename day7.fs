module Advent.Day7

open System

let test1 = "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"

let parseInput (text:string) = 
    let parseChildren (childrenText:string) =
        childrenText.Split(',') |> Seq.map (fun x -> x.Trim())

    let parseWeight (weightText:string) =
        weightText.Replace("(", "").Replace(")", "") |> int

    let parseLeftHandSide (text:string) =
        let split = text.Split()
        split.[0], parseWeight (split.[1])

    let leftAndRightSplit (text:string) = 
        text.Split([|"->"|], StringSplitOptions.None)

    let parseLeftAndRight (parts:string[]) = 
        (parseLeftHandSide parts.[0]),
        if Seq.length parts > 1 then (parseChildren parts.[1]) else Seq.empty

    text.Split('\n')|> Seq.map (leftAndRightSplit >> parseLeftAndRight)

let day7Calculator (text:string) = 
    let towerInfo = parseInput text
    
    let names = towerInfo |> Seq.map (fst >> fst)
    let haveParent = towerInfo |> Seq.collect (snd)
    let bottom = names |> Seq.where (fun x -> not (Seq.contains x haveParent)) |> Seq.head

    bottom
    

let day7 () =
    let puzzleInput = System.IO.File.ReadAllText("day7input.txt");
    
    printfn "%s" (day7Calculator test1)
    printfn "%s" (day7Calculator puzzleInput)