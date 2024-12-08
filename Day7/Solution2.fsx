open System.IO
open System

let input = 
    File.ReadAllLines("Day7/TestInput.txt")
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
        | Node of value:'T  * regularPlus: BST<'T> * combiPlus:BST<'T> * combiMultiply:BST<'T> * regularMultiply: BST<'T>

let rec createChildNodes value (list:uint64 list) =
    match list with
        | [] -> Empty
        | x::xs -> 
            let combinedValue = 
                match xs with
                | [] -> []
                | _ -> 
                    let combined = (string x + string xs.Head) |> uint64
                    [combined]
            Node(value, createChildNodes xs.Head xs, createChildNodes combinedValue.Head xs, createChildNodes combinedValue.Head xs, createChildNodes xs.Head xs)

let myTrees = 
    List.map (fun (x:uint64 * uint64 list) -> (fst x, createChildNodes (snd x).Head (snd x))) input

let rec exists targetValue acc bst =
    match bst with
    | Empty -> acc = targetValue
    | Node (x, regularPlus, combiPlus,combiMultiply, regularMultiply) ->
        printfn "x: %d acc: %d" x acc
        if acc > targetValue then false
        else 
        exists targetValue (x + acc) regularPlus 
            || exists targetValue (x + acc) combiPlus 
            ||exists targetValue (x * acc) combiMultiply 
            || exists targetValue (x * acc) regularMultiply

let result = 
    myTrees |> List.filter (fun x -> (exists (fst x) (uint64 0) (snd x)) = true) 
    // |> List.map (fun x -> printfn "%A" (fst x)  )
    |> List.sumBy fst