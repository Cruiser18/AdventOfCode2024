open System.IO
open System

let input = File.ReadAllText("Day9/Input.txt") |> Seq.toList

let toIntFromChar x = Char.GetNumericValue x |> int
let stringFromCharList (list:char array) =
    list |> Array.fold (fun acc x -> acc + string x) ""

let rec toFileAndSpace acc index isFile (list:char list) =
    match list with
    | [] -> acc
    | head::tail -> 
        match isFile with
        | true -> 
            let output = 
                List.init (toIntFromChar head) (fun x -> string index) 
                |> List.fold (fun acc x -> acc + string x) ""
            toFileAndSpace (acc + output) (index + 1) false tail
        | false ->
            let output = 
                List.init (toIntFromChar head) (fun x -> '.') 
                |> List.fold (fun acc x -> acc + string x) ""
            toFileAndSpace (acc + output) index true tail

let filesAndSpaces = toFileAndSpace "" 0 true input

let findEarliestDotIndex (x:char array) =
    Array.findIndex (fun x -> x = '.') x

let findLastNotDotIndex (x:char array) =
    Array.findIndexBack (fun x -> x <> '.') x

let allDotsUsed (x:string) = 
    let returnValue =
        x.TrimEnd('.') |> Seq.exists (fun x -> x = '.')
    not returnValue

let rec moveFileBlocks (list:char array) =
    printfn "Still running..."
    match list with
    | x when allDotsUsed (stringFromCharList x) = true -> stringFromCharList list
    | _ ->
        // printfn "List is %A" list
        let dotIndex = findEarliestDotIndex list
        let notDotIndex = findLastNotDotIndex list
        list.[dotIndex] <- list.[notDotIndex]
        list.[notDotIndex] <- '.'
        moveFileBlocks list

let test = 
    (moveFileBlocks (Seq.toArray filesAndSpaces)).TrimEnd('.')
    |> Seq.mapi (fun i x -> i * toIntFromChar x) 
    |> Seq.sum

// This is running way too inefficiently. I need to find a way to make this run faster.