open System.IO
open System

// let testList1 = [
//     [1;2;3];
//     [4;5;6];
//     [7;8;9]
// ]
// let testList2 = [7;8;9]
// let testList3 = testList1 @ testList2


let input = File.ReadAllLines("Day6/Input.txt")
let charArrayArray = 
    input
    |> Array.map (fun line -> line.ToCharArray())
let rows = charArrayArray.Length
let cols = charArrayArray.[0].Length
let array2D = Array2D.init rows cols (fun i j -> charArrayArray.[i].[j])

type CheckInFront = Clear | Obstacle | OutOfBounds | X | Loop
type Position = Position of int * int
type GuardPosition = GuardPosition of Position | NotPresentOnMap
type Direction = Up | Down | Left | Right

let getGuardPosition guard = 
    match guard with
    | GuardPosition(Position(x, y)) -> (x, y)
    | NotPresentOnMap -> (-1, -1)

let getPossibleArray2dValue x y (map:char array2d) =
    let maybeValue =
        try 
            Some(map.[x, y])
        with 
        | :? IndexOutOfRangeException as ex -> None
    match maybeValue with
    | Some value -> value
    | None -> ' '

let findGuard map =
    let mutable foundPosition = None
    for i in 0 .. Array2D.length1 map - 1 do
            for j in 0 .. Array2D.length2 map - 1 do
                if map.[i, j] = '^' then
                    foundPosition <- Some(Position(i, j))
    match foundPosition with
    | Some x -> GuardPosition x
    | None -> NotPresentOnMap

let mutable obstacleCounter = 0

let getTileInFront guard direction map previousTileInFront = 
    let guardX, guardY = getGuardPosition guard
    let getTile (x:int) (y:int) (map:char array2d) =
        let tileInFront = getPossibleArray2dValue x y map
        match tileInFront with
        | '#' -> Obstacle
        | 'O' ->
            obstacleCounter <- obstacleCounter + 1
            match obstacleCounter with
            | x when x > 4  ->
                obstacleCounter <- 0
                Loop
            | _ -> Obstacle
        | '.' -> Clear
        | 'X' -> X
        | _ -> 
            obstacleCounter <- 0
            OutOfBounds
    match direction with
    | Up -> getTile (guardX - 1) guardY map
    | Right -> getTile guardX (guardY + 1) map
    | Down -> getTile (guardX + 1) guardY map
    | Left -> getTile guardX (guardY - 1) map

let move guard (map:char array2d) direction =
    let guardX, guardY = getGuardPosition guard
    match direction with
    | Up -> 
        map.[guardX - 1, guardY] <- '^'
        map.[guardX, guardY] <- 'X'
        map
    | Right -> 
        map.[guardX, guardY + 1] <- '^'
        map.[guardX, guardY] <- 'X'
        map
    | Down -> 
        map.[guardX + 1, guardY] <- '^'
        map.[guardX, guardY] <- 'X'
        map
    | Left -> 
        map.[guardX, guardY - 1] <- '^'
        map.[guardX, guardY] <- 'X'
        map

let timer = System.Diagnostics.Stopwatch.StartNew()

let rec run map direction previousTileInFront =
    let guard = findGuard map
    // printfn "Tile: %A" previousTileInFront
    // printfn "Direction: %A" direction
    if timer.ElapsedMilliseconds > 2000 then 
        timer.Restart()
        Some(map)
    else
    match direction with 
    | Up ->
        let tileInFront = getTileInFront guard direction map previousTileInFront
        match tileInFront with
        | Loop -> Some(map)
        | Clear | X -> 
            let updatedMap = move guard map direction
            run updatedMap Up tileInFront
        | Obstacle -> run map Right tileInFront
        | OutOfBounds -> None
    | Right ->
        let tileInFront = getTileInFront guard direction map previousTileInFront
        match tileInFront with
        | Loop -> Some(map)
        | Clear | X  -> 
            let updatedMap = move guard map direction
            run updatedMap Right tileInFront
        | Obstacle -> run map Down tileInFront
        | OutOfBounds -> None
    | Down -> 
        let tileInFront = getTileInFront guard direction map previousTileInFront
        match tileInFront with
        | Loop -> Some(map)
        | Clear | X  -> 
            let updatedMap = move guard map direction
            run updatedMap Down tileInFront
        | Obstacle -> run map Left tileInFront
        | OutOfBounds -> None
    | Left -> // Move left
        let tileInFront = getTileInFront guard direction map previousTileInFront
        match tileInFront with
        | Loop -> Some(map)
        | Clear | X  -> 
            let updatedMap = move guard map direction
            run updatedMap Left tileInFront
        | Obstacle -> run map Up tileInFront
        | OutOfBounds -> None

let mapsWithObstacles = 
    let array2DWithObstacles map =
        let lines = 
            File.ReadAllLines("Day6/Output.txt")
            |> Array.map (fun line -> 
                let parts = line.Split(',', StringSplitOptions.TrimEntries) 
                (int parts.[0], int parts.[1]))
        
        let mutable listOfMaps = Array.init lines.Length (fun _ -> map)

        Array.iteri (fun i input ->
            let x, y = input
            let newMap = Array2D.copy map
            newMap.[x, y] <- 'O'
            listOfMaps[i] <- newMap) lines    
        listOfMaps
    array2DWithObstacles array2D

printfn "Maps with obstacles: %A" mapsWithObstacles.Length

let result = 
    let maps = mapsWithObstacles
    printfn "Maps: %A" maps.Length
    Array.mapi (fun i map -> 
        printfn "Map: %A" i
        // printfn "Map: %A" map
        if i = 39 then
            File.AppendAllLines ("Day6/Map.txt", [| sprintf "%A" map |])
        run map Up Clear) maps
    
// printfn "%A" result
let mutable counter = 0
Array.iter (fun map -> 
    match map with
    | Some x -> counter <- counter + 1
    | None -> printfn "No loop found") result

counter





// Always plus final results with 1 
// because the guard is not present in the final map