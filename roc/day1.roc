app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import "day1.txt" as input : Str

parseId = \elem ->
    clean = Str.trim elem
    Str.toI32 clean

parseElem = \elem ->
    { before, after } = Str.splitFirst? elem " "
    left = parseId? before
    right = parseId? after
    Ok (left, right)

transposeList : List (Result ok err) -> Result (List ok) err
transposeList = \list ->
    when list is
        [Ok first, .. as tail] ->
            rest = transposeList? tail
            Ok (List.concat [first] rest)

        [Err first, ..] -> Err first
        [] -> Ok []

parseInput = \data ->
    Str.splitOn data "\n"
    |> List.map parseElem
    |> transposeList

part1 =
    ids = parseInput? input
    lhs =
        ids
        |> List.map \elem -> elem.0
        |> List.sortAsc
    rhs =
        ids
        |> List.map \elem -> elem.1
        |> List.sortAsc

    List.map2 lhs rhs Num.absDiff
    |> List.sum
    |> Ok

incrementOrSet = \value ->
    when value is
        Ok v -> Ok (v + 1)
        Err Missing -> Ok 1

getOrDefault = \d, key ->
    elem = Dict.get d key
    when elem is
        Ok v -> v
        Err _ -> 0

part2 =
    ids = parseInput? input
    
    lhs =
        ids
        |> List.map \elem -> elem.0
    rhs =
        ids
        |> List.map \elem -> elem.1

    getCounts = \source ->
        when source is
            [first, .. as tail] -> 
                getCounts tail 
                |> Dict.update first incrementOrSet
            [] -> Dict.empty {}

    counts = getCounts rhs
    lhs 
    |> List.map \id -> id * (getOrDefault counts id)
    |> List.sum
    |> Ok
    

asExit = \result ->
    when result is
        Ok a ->
            Stdout.line! "$(Inspect.toStr a)"
            Task.ok {}

        Err e -> Task.err (Exit 1 "Error: $(Inspect.toStr e)")

run = part2

main = run |> asExit
