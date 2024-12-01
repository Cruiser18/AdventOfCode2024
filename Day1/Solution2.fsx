open System.IO
open System.Text.RegularExpressions

let lines = 
    File.ReadAllLines("Day1/Input.txt")
    |> Array.toList

let splitLine (line: string) =
    let split = Regex.Split(line, @"\s+")
    (split[0], split[1])

let splitLines = List.map splitLine lines
let firstResults = 
    List.map (fun (x,y) -> x) splitLines
    |> List.map (fun x -> int x)
    |> List.sort
let secondResults = 
    List.map (fun (x,y) -> y) splitLines
    |> List.map (fun x -> int x)
    |> List.sort

let findSimilarityScore x =
    let numResults =
        List.filter (fun y -> y = x) secondResults
        |> List.length
    x * numResults

let result = List.map findSimilarityScore firstResults |> List.sum
result

// Correct answer was 23228917