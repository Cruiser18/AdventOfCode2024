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

let rec sortList index (list:string list) =
    
    let currentNumber = list.[index]
    let rules = List.filter (fun (a,b) -> a = currentNumber) orderingRules
    let element =
        rules |> 
        List.tryFind (fun (a,b) ->
            let innerIndex = List.tryFindIndex (fun x -> x = b) list
            match innerIndex with
            | Some x when x < index -> true
            | None -> false
            | _ -> false
    )
    match element with
    | Some (a,b) -> 
        let breakingElementIndex = List.findIndex (fun x -> x = b) list
        let newList = 
            List.updateAt breakingElementIndex currentNumber list
            |> List.updateAt index b
        // printfn "%A" newList
        sortList breakingElementIndex newList
    | None -> 
        // printfn "%A" index
        match (index = list.Length-1) with
        | false -> sortList (index+1) list
        | true -> list

let result = sortList 0 ["97";"13";"75";"29";"47"]

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
                        let index = List.tryFindIndex (fun z -> z = b) pages
                        match index with
                        | Some y when y > i -> true
                        | None -> true
                        | _ -> false
                    ) 
                isCorrect
            )
        // printfn "%A" checkedPages
        List.exists (fun x -> x = false) checkedPages
    )

Math.Ceiling(float 5 / float 2)

let middleNumbersSum =
    correctPageOrderings
    |> List.map (fun pages -> 
        let sorted = sortList 0 pages
        let middleIndex = Math.Floor(float sorted.Length / float 2) |> int
        sorted.[middleIndex]
    )
    |> List.sumBy int

// Correct answer was 4905