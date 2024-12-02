open System.IO
open System

let lines = 
    File.ReadAllLines("Day2/TestInput.txt")
    |> Array.toList
    |> List.map (fun x -> 
        x.Split(' ')
        |> Array.map (fun x -> int(x))
    )

type IsSafe = Safe | Unsafe
type determineSafety = List<int> -> bool -> IsSafe

let rec determineSafety (line: List<int>) wasIncrement =
    match wasIncrement with
    | true ->
        match line with
        | [] -> Safe
        | x::xs when (x + xs.Head) < 1  -> Unsafe
        | x::xs when (x + xs.Head) > 3 -> Unsafe
    | false ->
        match line with
        | [] -> Safe
        | x::xs when (x - xs.Head) < 1  -> Unsafe
        | x::xs when (x - xs.Head) > 3 -> Unsafe
    
        

let result = 
    determineSafety [1;2;3;4] false