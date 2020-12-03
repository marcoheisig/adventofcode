#!/usr/bin/env -S dotnet fsi
open System.IO
open System.Text.RegularExpressions

// Utilities for Pattern Matching

let (|Integer|_|) (str: string) =
    match System.Int32.TryParse str with
        | true, int -> Some int
        | _ -> None

let (|Character|_|) (str:string) =
    if str.Length = 1
    then Some(str.[0])
    else None

let (|RegexGroups|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some([for x in m.Groups -> x.Value])
    else None

// The Actual Solution

type policy = int -> int -> char -> string -> bool

let validLine (policy: policy) line =
    match line with
        | RegexGroups "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)"
            [_; Integer lo; Integer hi; Character char; password] ->
            policy lo hi char password
        | _ -> false

let validPasswords policy =
    System.IO.File.ReadLines "input" |>
    Seq.filter (validLine policy) |>
    Seq.length

let policy1 lo hi char (password: string) =
    let n = String.length(String.filter ((=) char) password)
    lo <= n && n <= hi

let policy2 lo hi char (password: string) =
    match char = password.[lo-1], char = password.[hi-1] with
        | true, true -> false
        | true, false -> true
        | false, true -> true
        | false, false -> false

printfn "%d" (validPasswords policy1)

printfn "%d" (validPasswords policy2)
