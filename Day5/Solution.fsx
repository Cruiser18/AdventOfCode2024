// Foreach page, find out which has the correct ordering from the instructions
// After that, foreach page with correct ordering, get the middle number
// Add all the middle numbers together

open System.IO
open System

let lines = File.ReadAllLines("Day5/TestInput.txt") |> Array.toList

// split list at index with empty string
