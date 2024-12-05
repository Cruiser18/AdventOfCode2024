open System.IO
open System.Text.RegularExpressions

let lines = File.ReadAllText("Day3/Input.txt")
    
printfn "%A" lines.Length

let remove = @"don't\(\)[\w\W]*?(do\(\)|$)";
let regexRemove = new Regex(remove);

let enabledContent = regexRemove.Replace(lines, "");

printfn "%A" enabledContent

let results = Regex.Matches(enabledContent, @"mul\([0-9]+,[0-9]+\)")
let sumOfMults = 
    results 
    |> Seq.cast
    |> Seq.map (fun (regMatch:Match) -> 
        regMatch.Value)
    |> Seq.toList
    |> List.map (fun x -> 
        Regex.Match(x, @"[0-9]+,[0-9]+")
        |> fun (regMatch:Match) -> 
            let value = regMatch.Value.Split(',')
            (int(value.[0]) * int(value.[1]))        
    )
    |> List.sum

// 57382797 was not correct