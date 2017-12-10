module Advent.Day6

let puzzleInput = "10	3	15	10	5	15	5	15	9	2	5	8	5	2	3	6"

let test1 = "0 2 7 0"

let day5Calculator (text:string) = 
    let rec reallocate banks seen cycles =
        let newBanks = Array.copy banks

        let bigBankIndex, bigBankVal = newBanks |> Seq.mapi (fun i v -> i, v) |> Seq.maxBy snd

        newBanks.[bigBankIndex] <- 0

        let next curr = 
            if curr = newBanks.Length - 1 then 0 else curr + 1

        let rec reallocateCycle pointer remaining =
            if remaining = 0 then ()
            else
                newBanks.[pointer] <- newBanks.[pointer] + 1
                reallocateCycle (next pointer) (remaining - 1)

        reallocateCycle (next bigBankIndex) bigBankVal |> ignore        

        match Map.tryFind newBanks seen with
        | Some cyleWhenSeen -> cycles, cycles - cyleWhenSeen
        | None -> reallocate newBanks (Map.add newBanks cycles seen) (cycles + 1)            

    let banks = text.Split() |> Array.map int
    reallocate banks Map.empty 1
    

let day6 () =
    printfn "%A" (day5Calculator test1)
    printfn "%A" (day5Calculator puzzleInput)