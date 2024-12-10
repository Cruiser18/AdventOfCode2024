open System.IO
open System

let input = File.ReadAllLines("Day10/Input.txt")
let charArrayArray = 
    input
    |> Array.map (fun line -> line.ToCharArray())

let rows = charArrayArray.Length
let cols = charArrayArray.[0].Length
let array2D = Array2D.init rows cols (fun i j -> charArrayArray.[i].[j])

let trailHeads =
    let mutable indexList = []
    Array2D.iteri (fun i j value -> 
        if value = '0' then
            indexList <- indexList @ [(i, j)]
    ) array2D
    indexList

let tryGettingValue x y (map:char array2d) =
    let maybeValue =
        try 
            Some(map.[x, y])
        with 
        | :? IndexOutOfRangeException as ex -> None
    match maybeValue with
    | Some value -> value
    | None -> '.'

let toIntFromChar x = Char.GetNumericValue x |> int
let getCharAbove x y (map:char array2d) = ((tryGettingValue (x-1) y map), (x-1, y))
let getCharRight x y (map:char array2d) = ((tryGettingValue x (y+1) map), (x, y+1))
let getCharBelow x y (map:char array2d) = ((tryGettingValue (x+1) y map), (x+1, y))
let getCharLeft x y (map:char array2d) = ((tryGettingValue x (y-1) map), (x, y-1))
let getAllValidNeighbors x y (map:char array2d) = 
    [getCharAbove x y map; getCharRight x y map; getCharBelow x y map; getCharLeft x y map]

let rec checkTrail (nextIndex:(int*int)) previous (map:char array2d) =
    let (x,y) = nextIndex
    let nextInt = (Array2D.get map x y) |> toIntFromChar |> int
    match nextInt with
    | 9 -> 
        1
    | z when z <= previous -> 0
    | _ -> 
        let validDestinationsCount = 
            getAllValidNeighbors x y map 
            |> List.filter (fun (x, (y, z)) -> x <> '.')
            |> List.filter (fun (x, (y, z)) -> (toIntFromChar x) = (nextInt + 1))
            |> List.map (fun (x, (y, z)) -> checkTrail (y, z) nextInt map)
        List.sum validDestinationsCount

let result = 
    List.map (fun (x, y) -> checkTrail (x, y) -1 (Array2D.copy array2D)) trailHeads
    |> List.sum