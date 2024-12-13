open System.IO
open System

let input = File.ReadAllLines("Day12/Input.txt")
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

let mutable totalArea = 0
let mutable totalPerim = 0
let mutable allPreviousPositions = []

let findAllConnectedIndex x y (map:char array2d) =
    let mutable internalPreviousPositions = []
    let rec expandRegionInternal (currentPosition:(int*int)) =
        let (x,y) = currentPosition
        match currentPosition with
        | x when List.contains x internalPreviousPositions -> ()
        | _ ->
            internalPreviousPositions <- internalPreviousPositions @ [currentPosition]
            let allNeighbors = getAllNeighbors x y map
            let currentPositionValue = map.[x, y]
            let possibleNextPositions = 
                allNeighbors
                |> List.filter (fun (value, pos) -> value = currentPositionValue)
                |> List.filter (fun (_, pos) -> List.contains pos internalPreviousPositions |> not)
            match possibleNextPositions with
            | [] -> ()
            | _ ->
                possibleNextPositions |> List.iter (fun (value, position) -> expandRegionInternal position)
                ()
    expandRegionInternal (x, y) |> ignore
    internalPreviousPositions

let mutable totalPrice = 0

while allPreviousPositions.Length < inputMap.Length do
    let nextViablePosition = 
        [for i in 0..inputMap.GetLength(0)-1 do
            for j in 0..inputMap.GetLength(1)-1 do
                if List.contains (i, j) allPreviousPositions |> not then
                    yield (i, j)]
        |> List.head
    // printfn "Next Viable Position: %A" nextViablePosition
    // printfn "allPreviousPositions: %A" allPreviousPositions
    let (x,y) = nextViablePosition
    let listOfIds = findAllConnectedIndex x y inputMap
    allPreviousPositions <- allPreviousPositions @ listOfIds
    let area = listOfIds.Length
    let currentPositionValue = inputMap.[x, y]
    let perimeterCount = 
        listOfIds |> List.fold (fun (perimCount:int) (x, y) -> 
        let perimeterCount = 
            let allNeighbors = getAllNeighbors x y inputMap
            allNeighbors
            |> List.filter (fun (value, (x, y)) -> value <> currentPositionValue)
            |> List.length
        perimCount + perimeterCount
        ) 0
    totalPrice <- totalPrice + (area * perimeterCount)