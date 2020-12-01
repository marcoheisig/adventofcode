#!/usr/bin/env -S dotnet fsi
open System.IO

// Why does (::) result in a syntax error?
let cons x xs = x::xs

let rec combinations n l =
    match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map (cons x) (combinations (k-1) xs) @ combinations k xs

let sumFilter n =
    fun l -> (List.reduce (+) l) = n

let input =
    List.ofSeq (Seq.map int (System.IO.File.ReadLines "input"))

let solveDay1 n =
    List.filter (sumFilter 2020) (combinations n input) |>
    List.head |>
    List.reduce (*)

printfn "%d" (solveDay1 2)

printfn "%d" (solveDay1 3)
