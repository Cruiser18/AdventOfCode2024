open System.IO
open System

let lines = 
    File.ReadAllLines("Day2/Input.txt")
    |> Array.toList
    |> List.map (fun x -> 
        x.Split(' ')
        |> Array.map (fun x -> int(x))
        |> Array.toList
    )

type IsSafe = Safe | Unsafe

let getDifference x y = 
    match x with
    | x when x > y -> x - y
    | _ -> y - x

let shouldIncrement x =
    match x with
        | x::xs when x > xs.Head -> false
        | _ -> true

let rec determineSafety isIncrementing (line: List<int>) =
    printfn "%A" line
    match isIncrementing with
    | true ->
        match line with
        | [_] -> Safe
        | [] -> Safe
        | x::xs when (shouldIncrement [x;xs.Head]) = false -> Unsafe
        | x::xs when (getDifference x xs.Head) < 1  -> Unsafe
        | x::xs when (getDifference x xs.Head) > 3 -> Unsafe
        | _::xs -> determineSafety isIncrementing xs
    | false ->
        match line with
        | [_] -> Safe
        | [] -> Safe
        | x::xs when (shouldIncrement [x;xs.Head]) = true -> Unsafe
        | x::xs when (getDifference x xs.Head) < 1  -> Unsafe
        | x::xs when (getDifference x xs.Head) > 3 -> Unsafe
        | _::xs -> determineSafety isIncrementing xs

let result = 
    List.map (fun x -> determineSafety  (shouldIncrement x) x) lines
    |> List.filter (fun x -> x = Safe)
    |> List.length

// Answer was 483