open System.IO
open System

let input = File.ReadAllText("Day11/TestInput.txt")

let split n =
    let num = n.ToString()
    let left = num[0 .. num.Length / 2 - 1].TrimStart([| '0' |])
    let right = num[num.Length / 2 ..].TrimStart([| '0' |])

    [| if left = "" then 0UL else System.UInt64.Parse left
       if right = "" then 0UL else System.UInt64.Parse right |]

let counts = input.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map uint64
let memo = Collections.Generic.Dictionary<uint64, uint64[]>()
let blink num =
    if memo.ContainsKey num then
        memo[num]
    else
        let result =
            match num with
            | 0UL -> [| 1UL |]
            | n when (int (System.Math.Floor(Math.Log10(float n))) + 1) % 2 = 0 -> split n
            | n -> [| n * 2024UL |]

        memo.Add(num, result)
        result

let mutable dict = Collections.Generic.Dictionary<uint64, uint64>()

printfn "Dict length: %d" dict.Count
Array.iter (fun x -> 
    match x with
    | x when dict.ContainsKey(x) -> 
        dict.[x] <- dict.[x] + 1UL
    | _ -> dict.[x] <- 1UL) counts

for _ in 1..6 do
    let new_dict = Collections.Generic.Dictionary<uint64, uint64>()
    
    dict.Keys |> Seq.iter (fun parentStone -> 
        (blink parentStone) |> Array.iter (fun childStone -> 
            new_dict.[childStone] <- 
                printfn "ParentStone: %A ChildStone: %A" parentStone childStone
                if new_dict.ContainsKey(childStone) then
                    printfn "ChildStoneDictValue: %A ParentStoneDictValue: %A" new_dict.[childStone] dict.[parentStone]
                    printfn "%A" (new_dict.[childStone] + dict.[parentStone])
                    new_dict.[childStone] + dict.[parentStone]
                else
                    printfn "Adding ParentStoneDictValue: %A from dict key %A" dict.[parentStone] parentStone
                    dict.[parentStone]))
            
    dict <- new_dict

let new_sum = dict.Values |> Seq.sum
printfn "Part 2: %d" new_sum