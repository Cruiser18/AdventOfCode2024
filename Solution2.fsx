open System.IO
open System.Text.RegularExpressions

let lines = 
    File.ReadAllLines("Day3/Input.txt")
    |> Array.reduce (fun x y -> x + y)
    
printfn "%A" lines.Length

let result1 = Regex.Matches(lines, @"don't\(\).+do\(\)")
// let result1 = Regex.Matches(lines, @"\S+?don't\(\)?")
// let result2 = Regex.Matches(lines, @"do\(\).+don't\(\)")
// let result3 = Regex.Matches(lines, @".*(do\(\).*)$")

// let matchCollectionToString =
//     fun (matchCollection:MatchCollection) ->
//         matchCollection
//         |> Seq.cast
//         |> Seq.map (fun (regMatch:Match) -> 
//             regMatch.Value)
//         |> Seq.toList
//         |> List.reduce (fun x y -> x + y)

// Foreach match I need to remove the match from the string
let matchCollectionToString = 
    fun (matchCollection:MatchCollection) ->
        matchCollection
        |> Seq.cast
        |> Seq.map (fun (regMatch:Match) -> 
            regMatch.Value)
        |> Seq.toList
        |> List.map (fun x -> 
            lines.Replace(x, ""))
        |> List.reduce (fun x y -> x + y)

// let allMatches = 
//     (matchCollectionToString result1) + (matchCollectionToString result2) + (matchCollectionToString result3)

let allMatches = matchCollectionToString result1

printfn "%A" lines

let results = Regex.Matches(allMatches, @"mul\([0-9]+,[0-9]+\)")
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

// 57382797 was not correct