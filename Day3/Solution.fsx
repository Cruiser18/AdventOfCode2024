open System.IO
open System.Text.RegularExpressions

let lines = 
    File.ReadAllLines("Day3/Input.txt")
    |> Array.reduce (fun x y -> x + y)

let results = Regex.Matches(lines, @"mul\([0-9]+,[0-9]+\)")
let sumOfMults = 
    results 
    |> Seq.cast
    |> Seq.map (fun (regMatch:Match) -> 
        regMatch.Value)
    |> Seq.toList
    |> List.map (fun x -> 
        Regex.Match(x, @"[0-9]+,[0-9]+")
        |> fun (regMatch:Match) -> 
            let value = regMatch.Value.Split(',')
            (int(value.[0]) * int(value.[1]))        
    )
    |> List.sum
