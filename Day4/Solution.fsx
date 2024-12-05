open System.IO
open System

let input = File.ReadAllLines("Day4/Input.txt")
let charArrayArray = 
    input
    |> Array.map (fun line -> line.ToCharArray())
let rows = charArrayArray.Length
let cols = charArrayArray.[0].Length
let array2D = Array2D.init rows cols (fun i j -> charArrayArray.[i].[j])
let getPossibleArray2dValue x y =
    let maybeValue =
        try 
            Some(array2D.[x, y])
        with 
        | :? IndexOutOfRangeException as ex -> None
    match maybeValue with
    | Some value -> value
    | None -> ' '

let result = 
    Array2D.mapi (fun i j v -> 
        let vertical =
            match v with
            | 'X' -> 
                match getPossibleArray2dValue i (j+1) with
                | 'M' -> 
                    match getPossibleArray2dValue i (j+2) with
                    | 'A' ->
                        match getPossibleArray2dValue i (j+3) with
                        | 'S' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | 'S' -> 
                match getPossibleArray2dValue i (j+1) with
                | 'A' -> 
                    match getPossibleArray2dValue i (j+2) with
                    | 'M' ->
                        match getPossibleArray2dValue i (j+3) with
                        | 'X' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | _ -> false
        let horizontal =
            match v with
            | 'X' -> 
                match getPossibleArray2dValue (i+1) j with
                | 'M' -> 
                    match getPossibleArray2dValue (i+2) j with
                    | 'A' ->
                        match getPossibleArray2dValue (i+3) j with
                        | 'S' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | 'S' -> 
                match getPossibleArray2dValue (i+1) j with
                | 'A' -> 
                    match getPossibleArray2dValue (i+2) j with
                    | 'M' ->
                        match getPossibleArray2dValue (i+3) j with
                        | 'X' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | _ -> false
        let diagonalDown =
            match v with
            | 'X' -> 
                match getPossibleArray2dValue (i+1) (j+1) with
                | 'M' -> 
                    match getPossibleArray2dValue (i+2) (j+2) with
                    | 'A' ->
                        match getPossibleArray2dValue (i+3) (j+3) with
                        | 'S' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | 'S' -> 
                match getPossibleArray2dValue (i+1) (j+1) with
                | 'A' -> 
                    match getPossibleArray2dValue (i+2) (j+2) with
                    | 'M' ->
                        match getPossibleArray2dValue (i+3) (j+3) with
                        | 'X' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | _ -> false
        let diagonalUp =
            match v with
            | 'X' -> 
                match getPossibleArray2dValue (i-1) (j+1) with
                | 'M' -> 
                    match getPossibleArray2dValue (i-2) (j+2) with
                    | 'A' ->
                        match getPossibleArray2dValue (i-3) (j+3) with
                        | 'S' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | 'S' -> 
                match getPossibleArray2dValue (i-1) (j+1) with
                | 'A' -> 
                    match getPossibleArray2dValue (i-2) (j+2) with
                    | 'M' ->
                        match getPossibleArray2dValue (i-3) (j+3) with
                        | 'X' -> true
                        | _ -> false
                    | _ -> false
                | _ -> false
            | _ -> false
        Array.filter (fun x -> x = true) [|vertical; horizontal; diagonalDown; diagonalUp|]
        |> Array.length
        ) array2D

let mutable counter = 0
Array2D.iter (fun x -> counter <- counter + x) result

// Answer was 2370