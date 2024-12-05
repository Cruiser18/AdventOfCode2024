open System.IO
open System

let lines = File.ReadAllLines("Day5/Input.txt") |> Array.toList
let splitIndex = List.findIndex (fun x -> x = "") lines
let orderingRules = lines.[0..splitIndex - 1] |> List.map (fun x -> x.Split('|')) |> List.map (fun x -> (x.[0], x.[1]))
let pageOrdering = 
    lines.[splitIndex + 1..] 
    |> List.map (fun x -> 
    x.Split(',')
    |> Array.toList)

let correctPageOrderings =
    pageOrdering
    |> List.filter (fun pages -> 
        let checkedPages = 
            pages |>
            List.mapi (fun i x -> 
                let orderings = List.filter (fun (a,b) -> a = x) orderingRules
                let isCorrect = 
                    orderings |>
                    List.forall (fun (a,b) -> 
                        // printfn "%A" i
                        // printfn "%A" a
                        // printfn "%A" b
                        let index = List.tryFindIndex (fun z -> z = b) pages
                        // printfn "%A" index
                        match index with
                        | Some y when y > i -> true
                        | None -> true
                        | _ -> false
                    ) 
                isCorrect
            ) 
        // printfn "%A" test
        List.forall (fun x -> x = false) checkedPages
    )

let middleNumbersSum =
    correctPageOrderings
    |> List.map (fun pages -> 
        let middleIndex = pages.Length / 2
        pages.[middleIndex]
    )
    |> List.sumBy int

// Correct answer was 4905