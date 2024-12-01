open System.IO
open System
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
    |> List.sort
let secondResults = 
    List.map (fun (x,y) -> y) splitLines
    |> List.sort

List.map2 (fun x y -> Math.Abs (int(x) - int(y))) firstResults secondResults
|> List.sum

// Correct answer was 1889772