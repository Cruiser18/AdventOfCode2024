open System.IO
open System

let input = 
    File.ReadAllLines("Day7/Input.txt")
    |> Array.map (fun x -> x.Split(":", StringSplitOptions.TrimEntries))
    |> Array.map (fun x -> 
        let values = 
            x.[1].Split(" ", StringSplitOptions.TrimEntries) 
            |> Array.map uint64
            |> Array.toList
        (uint64 x.[0], values))
    |> Array.toList

type BST<'T> =
        | Empty
        | Node of value:'T  * left: BST<'T> * right: BST<'T>

let rec createChildNodes (list:uint64 list) =
    match list with
        | [] -> Empty
        | x::xs -> 
            Node(x, createChildNodes xs, createChildNodes xs)

let myTrees = 
    List.map (fun (x:uint64 * uint64 list) -> (fst x, createChildNodes (snd x))) input

let rec exists targetValue acc bst =
    match bst with
    | Empty -> acc = targetValue
    | Node (x, left, right) ->
        // printfn "x: %d acc: %d" x acc
        if acc > targetValue then false
        else exists targetValue (x + acc) left || exists targetValue (x * acc) right

let result = 
    myTrees |> List.filter (fun x -> (exists (fst x) (uint64 0) (snd x)) = true) 
    // |> List.map (fun x -> printfn "%A" (fst x)  )
    |> List.sumBy fst

// 3351430526202L was wrong
// 3351424677624 was correct answer