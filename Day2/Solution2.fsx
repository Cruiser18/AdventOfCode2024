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

let rec determineSafety isIncrementing isBadlevelExpended (line: List<int>) =
    printfn "%A" line
    printfn "%A" isBadlevelExpended

    match isBadlevelExpended with
    | true ->
        match isIncrementing with
        | true ->
            match line with
            | [_] -> Safe
            | [] -> Safe
            | x::xs when (shouldIncrement [x;xs.Head]) <> isIncrementing -> 
                printfn "%A" isIncrementing
                printfn "%A" x
                printfn "%A" xs.Head
                printfn "%A" (shouldIncrement [x;xs.Head])
                Unsafe
            | x::xs when (getDifference x xs.Head) < 1  -> Unsafe
            | x::xs when (getDifference x xs.Head) > 3 -> Unsafe
            | x::xs -> determineSafety isIncrementing true xs
        | false ->
            match line with
            | [_] -> Safe
            | [] -> Safe
            | x::xs when (shouldIncrement [x;xs.Head]) <> isIncrementing -> Unsafe
            | x::xs when (getDifference x xs.Head) < 1  -> Unsafe
            | x::xs when (getDifference x xs.Head) > 3 -> Unsafe
            | _::xs -> determineSafety isIncrementing true xs
    | false ->
        printfn "%A" isIncrementing
        match isIncrementing with
        | true ->
            match line with
            | [_] -> Safe
            | [] -> Safe
            | x::xs when (shouldIncrement [x;xs.Head]) <> isIncrementing -> 
                match xs.Tail with
                | [] -> determineSafety (not isIncrementing) true xs
                | _ -> determineSafety (shouldIncrement [xs.Head;xs.Tail.Head]) true xs
            | x::xs when (getDifference x xs.Head) < 1  -> determineSafety isIncrementing true xs
            | x::xs when (getDifference x xs.Head) > 3 -> determineSafety isIncrementing true xs
            | _::xs -> determineSafety isIncrementing false xs
        | false ->
            match line with
            | [_] -> Safe
            | [] -> Safe
            | x::xs when (shouldIncrement [x;xs.Head]) <> isIncrementing -> 
                match xs.Tail with
                | [] -> determineSafety (not isIncrementing) true xs
                | _ -> determineSafety (shouldIncrement [xs.Head;xs.Tail.Head]) true xs
            | x::xs when (getDifference x xs.Head) < 1  -> determineSafety isIncrementing false xs
            | x::xs when (getDifference x xs.Head) > 3 -> determineSafety isIncrementing false xs
            | _::xs -> determineSafety isIncrementing false xs

let result = 
    List.map (fun x -> determineSafety  (shouldIncrement x) false x) lines
    |> List.filter (fun x -> x = Safe)
    |> List.length