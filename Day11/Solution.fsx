open System.IO
open System

let input = File.ReadAllText("Day11/Input.txt")
let zeroStoneToOne x = 1
let evenStoneToTwoStones x = 
    Seq.splitInto 2 (string x)
    |> Seq.map (fun x -> 
        let result = 
            Array.fold (fun acc x -> 
            let asstring = string x
            acc + asstring
            ) "" x
        // printfn "%A" result
        uint64 result)
    |> List.ofSeq

let defaultStone (x:uint64) = 
    x * 2024UL

let isEvenLength x = 
    (string x).Length % 2 = 0

let mutable resultHolder = input
for i = 1 to 75 do
    let splitResultHolder = resultHolder.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    resultHolder <- ""
    // printfn "%A" splitResultHolder
    Array.iter (fun (x:string) -> 
        let result = 
            // printfn "X: %A" x
            match (uint64 x) with
            | 0UL -> string (zeroStoneToOne x)
            | y when isEvenLength y -> 
                evenStoneToTwoStones y
                |> List.map (fun x -> string x)
                |> List.fold (fun (acc:string) x -> acc + " " + (string x)) ""
            | _ -> string (defaultStone (uint64 x))
        // printfn "Result: %A" (result.Trim())
        resultHolder <- resultHolder + " " + result.Trim()
    ) splitResultHolder

resultHolder.Split(" ", StringSplitOptions.RemoveEmptyEntries)
|> Array.length