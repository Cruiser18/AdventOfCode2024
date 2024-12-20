open System.IO
open System

let input = File.ReadAllLines("Day8/Input.txt")
let charArrayArray = 
    input
    |> Array.map (fun line -> line.ToCharArray())

let rows = charArrayArray.Length
let cols = charArrayArray.[0].Length
let array2D = Array2D.init rows cols (fun i j -> charArrayArray.[i].[j])
let resultMap = Array2D.init rows cols (fun i j -> '.')

let rec getTowerTypesAndPosition (map:char array2d) =
    let mutable foundTowers = []
    for (i:int) in 0 .. Array2D.length1 map - 1 do
        for (j:int) in 0 .. Array2D.length2 map - 1 do
            let value = map.[i, j]
            if value <> '.' then
                foundTowers <- foundTowers @ [map.[i, j], (i, j)]
    foundTowers

let towerTypesAndPosition = getTowerTypesAndPosition array2D

let tryPlacingValue x y (map:char array2d) =
    let maybeValue =
        try 
            Some(map.[x, y])
        with 
        | :? IndexOutOfRangeException as ex -> None
    match maybeValue with
    | Some value -> value
    | None -> ' '

let someFunction (towerTypeAndPosition:list<char * (int * int)>) =
    towerTypeAndPosition |> List.iter (fun (x:char * (int*int)) ->
        let towerType, position = x
        let otherTowers = towerTypeAndPosition |> List.filter (fun y -> fst y = towerType)
        otherTowers |> List.iter (fun y ->
            let otherTowerType, otherTowerPosition = y
            if position <> otherTowerPosition then
                let diffX = fst otherTowerPosition - (fst position)
                let diffY = snd otherTowerPosition - (snd position)
                let antiNodeX = fst otherTowerPosition + diffX
                let antiNodeY = snd otherTowerPosition + diffY
                let result = tryPlacingValue antiNodeX antiNodeY resultMap
                match result with
                | ' ' -> ()
                | _ -> resultMap.[antiNodeX, antiNodeY] <- '#'
        )
    )
    
someFunction towerTypesAndPosition

let countTowers map =
    let mutable foundTowers = 0
    for (i:int) in 0 .. Array2D.length1 map - 1 do
        for (j:int) in 0 .. Array2D.length2 map - 1 do
            let value = map.[i, j]
            if value = '#' then
                foundTowers <- foundTowers + 1
    foundTowers

let result = countTowers resultMap