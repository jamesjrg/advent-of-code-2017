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
        childrenText.Split(',') |> Seq.map (fun x -> x.Trim()) |> List.ofSeq

    let parseWeight (weightText:string) =
        weightText.Replace("(", "").Replace(")", "") |> int

    let parseLeftHandSide (text:string) =
        let split = text.Split()
        split.[0], parseWeight (split.[1])

    let leftAndRightSplit (text:string) = 
        text.Split([|"->"|], StringSplitOptions.None)

    let parseLeftAndRight (parts:string[]) = 
        let name, weight = parseLeftHandSide parts.[0]
        let children = if Seq.length parts > 1 then (parseChildren parts.[1]) else List.empty

        name, weight, children

    text.Split('\n')|> Seq.map (leftAndRightSplit >> parseLeftAndRight)

let getRootName towerInfo = 
    let names = towerInfo |> Seq.map (fun (name, _, _) -> name)
    let haveParent = towerInfo |> Seq.collect (fun (_, _, children) -> children)
    names |> Seq.find (fun x -> not (Seq.contains x haveParent))

let partOne (text:string) = 
    let towerInfo = parseInput text
    
    getRootName towerInfo

type Node = { name: string; weight: int; children: string list }

type UnbalancedNameAndSuggestedChange = { name: string; suggestedChange: int }

type Result = { nameOfUnbalanced: UnbalancedNameAndSuggestedChange option; totalWeight: int }

let partTwo (text: string) = 
    let towerInfo = parseInput text
    let nodeMap = towerInfo |> Seq.map (fun (name, weight, children) -> name, {name = name; weight = weight; children = children}) |> Map.ofSeq

    let rec doCalc (current: Node) =
        let children = current.children |> Seq.map (fun x -> nodeMap.[x])

        let childResults = children |> Seq.map doCalc |> Seq.zip children |> List.ofSeq

        let totalWeight = (childResults |> List.sumBy (fun (_, y) -> y.totalWeight)) + current.weight

        let nameOfUnbalanced =
            match childResults |> List.tryFind (fun (_, y) -> y.nameOfUnbalanced.IsSome) with
            | Some (node, result) -> result.nameOfUnbalanced
            | _ ->
                let childrenWithUniqueWeight =
                    childResults |> List.groupBy (fun (_, y) -> y.totalWeight) |> List.map snd |> List.filter (fun v -> Seq.length v = 1)

                match childrenWithUniqueWeight with
                | [] -> None
                | h :: t ->
                    let node, result = List.head h
                    let _, anotherResult = List.find (fun ((anotherNode:Node), _) -> anotherNode.name <> node.name) childResults
                    
                    Some {name = node.name; suggestedChange = node.weight - (result.totalWeight - anotherResult.totalWeight) }

        { nameOfUnbalanced = nameOfUnbalanced; totalWeight = totalWeight }

    
    let rootName = getRootName towerInfo
    doCalc nodeMap.[rootName]

let day7 () =
    let puzzleInput = System.IO.File.ReadAllText("day7input.txt");

    let doIt input =
        let partTwoResult = partTwo input
        printfn "%d %s %d" partTwoResult.totalWeight partTwoResult.nameOfUnbalanced.Value.name partTwoResult.nameOfUnbalanced.Value.suggestedChange
    
    doIt test1
    doIt puzzleInput
