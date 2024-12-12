open System.IO
open System

let input = File.ReadAllLines("Day12/TestInput.txt")
let charArrayArray = 
    input
    |> Array.map (fun line -> line.ToCharArray())

let rows = charArrayArray.Length
let cols = charArrayArray.[0].Length
let inputMap = Array2D.init rows cols (fun i j -> charArrayArray.[i].[j])

let tryGettingValue x y (map:char array2d) =
    let maybeValue =
        try 
            Some(map.[x, y])
        with 
        | :? IndexOutOfRangeException as ex -> None
    match maybeValue with
    | Some value -> value
    | None -> '.'
let getCharAbove x y (map:char array2d) = ((tryGettingValue (x-1) y map), (x-1, y))
let getCharRight x y (map:char array2d) = ((tryGettingValue x (y+1) map), (x, y+1))
let getCharBelow x y (map:char array2d) = ((tryGettingValue (x+1) y map), (x+1, y))
let getCharLeft x y (map:char array2d) = ((tryGettingValue x (y-1) map), (x, y-1))

let getAllNeighbors x y (map:char array2d) = 
    [getCharAbove x y map; getCharRight x y map; getCharBelow x y map; getCharLeft x y map]


let mutable allPreviousPositions = []
let mutable totalArea = 0
let mutable totalPerim = 0
let expandRegion x y =
    let rec expandRegionInternal (visitedPositions:(int*int) list) (currentPosition:(int*int)) =
        let (x,y) = currentPosition
        let newVisitedPositions = visitedPositions @ [currentPosition]
        allPreviousPositions <- allPreviousPositions @ [currentPosition]
        printfn "Current Position: %A" (x, y)
        let allNeighbors = getAllNeighbors x y inputMap
        printfn "All Neighbors: %A" allNeighbors
        let currentPositionValue = inputMap.[x, y]
        // let perimeterCount = 
        //     allNeighbors
        //     |> List.filter (fun (value, (x, y)) -> value <> currentPositionValue)
        //     |> List.length
        let possibleNextPositions = 
            allNeighbors
            |> List.filter (fun (value, pos) -> value = currentPositionValue)
            |> List.filter (fun (_, pos) -> List.contains pos allPreviousPositions |> not)
        match possibleNextPositions with
        | [] -> 
            newVisitedPositions
        | _ ->
            // let positions = previousPositions |> List.map (fun (a, b) -> (a, b))
            // allPreviousPositions <- allPreviousPositions @ positions
            let result =
                possibleNextPositions |> 
                List.fold (fun (value, (a, b)) -> 
                    let list = expandRegionInternal newVisitedPositions (a, b)
                    printfn "Total Area: %A Total Perim: %A" totalArea totalPerim
                    value @ list
                ) []
            
    expandRegionInternal 1 0 (x, y)

//Foreach item in inputMap, if it's not in allPreviousPositions, expandRegion
let result =
    Array2D.mapi (fun i j value ->
        if List.contains (i, j) allPreviousPositions |> not then
            expandRegion i j // I'm going about this the wrong way. I should get all the connected tiles, 
            //then the area is the count of those tiles, 
            //and the perimeter is the count of the tiles where a neighbor is not the same as the current tile
        else
            (-1, -1)
    ) inputMap
printfn "%A" result
let mutable totalPrice = 0
result |> Array2D.iter (fun v -> 
    match v with
    | (-1, _) | (_, -1) -> ()
    | _ ->
        printfn "Area: %A perim: %A" (fst v) (snd v)
        totalPrice <- totalPrice + (fst v) * (snd v)
)