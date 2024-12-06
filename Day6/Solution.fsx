open System.IO
open System

let input = File.ReadAllLines("Day6/Input.txt")
let charArrayArray = 
    input
    |> Array.map (fun line -> line.ToCharArray())
let rows = charArrayArray.Length
let cols = charArrayArray.[0].Length
let array2D = Array2D.init rows cols (fun i j -> charArrayArray.[i].[j])

type CheckInFront = Clear | Obstacle | OutOfBounds
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

let getTileInFront guard direction map = 
    let guardX, guardY = getGuardPosition guard
    let getTile (x:int) (y:int) (map:char array2d) =
        let tileInFront = getPossibleArray2dValue x y map
        match tileInFront with
        | '#' -> Obstacle
        | '.' -> Clear
        | 'X' -> Clear
        | _ -> OutOfBounds
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

let rec run map direction =
    let guard = findGuard map
    match direction with 
    | Up ->
        let tileInFront = getTileInFront guard direction map
        match tileInFront with
        | Clear -> 
            let updatedMap = move guard map direction
            run updatedMap Up
        | Obstacle -> run map Right
        | OutOfBounds -> map
    | Right ->
        let tileInFront = getTileInFront guard direction map
        match tileInFront with
        | Clear -> 
            let updatedMap = move guard map direction
            run updatedMap Right
        | Obstacle -> run map Down
        | OutOfBounds -> map
    | Down -> 
        let tileInFront = getTileInFront guard direction map
        match tileInFront with
        | Clear -> 
            let updatedMap = move guard map direction
            run updatedMap Down
        | Obstacle -> run map Left
        | OutOfBounds -> map
    | Left -> // Move left
        let tileInFront = getTileInFront guard direction map
        match tileInFront with
        | Clear -> 
            let updatedMap = move guard map direction
            run updatedMap Left
        | Obstacle -> run map Up
        | OutOfBounds -> map

let sumOf2DArray array2D =
    let mutable sum = 0
    for i in 0 .. Array2D.length1 array2D - 1 do
        for j in 0 .. Array2D.length2 array2D - 1 do
            if array2D.[i, j] = 'X' then
                sum <- sum + 1
    sum

let finalMap = run array2D Up
sumOf2DArray finalMap

// Correct answer is 5305