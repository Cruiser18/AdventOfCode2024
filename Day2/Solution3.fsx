open System.IO
open System

let lines = 
    File.ReadAllLines("Day2/TestInput.txt")
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

let rec determineSafety isIncrementing (index:int) (line: List<int>) =
    printfn "%A" line
    match isIncrementing with
    | true ->
        match line with
        | [_] -> (Safe, index)
        | [] -> (Safe, index)
        | x::xs when (shouldIncrement [x;xs.Head]) = false -> (Unsafe, index + 1)
        | x::xs when (getDifference x xs.Head) < 1  -> (Unsafe, index + 1)
        | x::xs when (getDifference x xs.Head) > 3 -> (Unsafe, index + 1)
        | _::xs -> determineSafety isIncrementing (index + 1) xs
    | false ->
        match line with
        | [_] -> (Safe, index)
        | [] -> (Safe, index)
        | x::xs when (shouldIncrement [x;xs.Head]) = true -> (Unsafe, index + 1)
        | x::xs when (getDifference x xs.Head) < 1  -> (Unsafe, index + 1)
        | x::xs when (getDifference x xs.Head) > 3 -> (Unsafe, index + 1)
        | _::xs -> determineSafety isIncrementing (index + 1) xs

let removeItemAtIndex index list = List.removeAt index list

let checkEachItem line =
    // For each int in line, remove it and check if it's safe
    // If the line is unsafe two then mark as unsafe

let checkLine line =
    let test = checkEachItem line
    
    let isSafe = (determineSafety  (shouldIncrement line) 0 line)
    match isSafe with
    | (Safe, y) -> (Safe, 0)
    | (Unsafe, y) -> (determineSafety (shouldIncrement line) 0 (removeItemAtIndex y line))

let result = 
    List.map checkLine lines
    // |> List.filter (fun (isSafe,index) -> isSafe = Safe)
    // |> List.length

// For future fix, try brute forcing this by for each line, remove an item and check if it's safe