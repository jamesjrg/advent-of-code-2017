module Advent.Day8

open FParsec
open System.Collections.Generic
open System
open System.Reflection.Metadata

let test1 = "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"

type IncOrDec = Inc | Dec

type Comparison = 
    Equals | NotEqualTo | GreaterThan | LessThan | GreaterOrEqualTo | LessThanOrEqualTo

type Instruction = {register: string; incOrDec: IncOrDec; amount: int; testRegister: string; comparison: Comparison; toCompare: int }

let registerName = charsTillString " " true 10

let incOrDec = (stringReturn "inc" Inc) <|> (stringReturn "dec" Dec)

let comparison =
    (stringReturn "==" Equals)
    <|> (stringReturn "!=" NotEqualTo)
    <|> (stringReturn ">=" GreaterOrEqualTo)
    <|> (stringReturn "<=" LessThanOrEqualTo)
    <|> (stringReturn ">" GreaterThan)
    <|> (stringReturn "<" LessThan)

let pipe6 p1 p2 p3 p4 p5 p6 f =
    pipe4 p1 p2 p3 (tuple3 p4 p5 p6)
          (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

let instructionParser =
    pipe6 registerName (incOrDec .>> spaces1) (pint32 .>> (skipString " if ")) registerName (comparison .>> spaces1)  pint32
        (fun a b c d e f -> {register = a; incOrDec = b; amount = c; testRegister = d; comparison = e; toCompare = f})

let resultOrException = function
    | Success (result, _, _) -> result
    | Failure (x, y, z) -> failwith ""

let checkCondition comparison toCompare value =
    match comparison with
        | Equals -> value = toCompare
        | NotEqualTo -> value <> toCompare
        | GreaterThan -> value > toCompare
        | LessThan -> value < toCompare
        | GreaterOrEqualTo -> value >= toCompare
        | LessThanOrEqualTo -> value <= toCompare

let doIncDec incOrDec amount value =
    match incOrDec with 
        | Inc -> value + amount
        | Dec -> value - amount

let runInstruction (ins:Instruction) (registers:Dictionary<string, int>) =
    let _, value = registers.TryGetValue ins.register
    let _, testValue = registers.TryGetValue ins.testRegister

    if checkCondition ins.comparison ins.toCompare testValue then
        registers.[ins.register] <- doIncDec ins.incOrDec ins.amount value

let doProcess (text:string) =
    let registers = Dictionary<string, int>()
    let instructions = text.Split('\n') |> Seq.map ((fun x -> (run instructionParser x)) >> resultOrException) |> List.ofSeq

    let rec runInstructions instructions biggestEver =
        match instructions with
        | [] -> biggestEver 
        | h :: t ->
            runInstruction h registers
            let newBiggest = if registers.Count <> 0 then (max (Seq.max registers.Values) biggestEver) else 0
            runInstructions t newBiggest
    
    let biggestEver = runInstructions instructions 0

    (Seq.max registers.Values), biggestEver

let day8 () =
    let puzzleInput = System.IO.File.ReadAllText("day8input.txt");

    let biggest, biggestEver = doProcess puzzleInput
    printfn "%d %d" biggest biggestEver
