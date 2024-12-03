module [asExit, getOrDefault, transposeList, lines]

import pf.Stdout

lines : Str -> List Str
lines = \in ->
    in
    |> Str.splitOn "\n"
    |> List.map Str.trim

asExit : Result a e -> Task {} _ where a implements Inspect, e implements Inspect
asExit = \result ->
    when result is
        Ok a ->
            Stdout.line! "$(Inspect.toStr a)"
            Task.ok {}

        Err e -> Task.err (Exit 1 "Error: $(Inspect.toStr e)")

getOrDefault : Dict a (Int b), a -> Int b where a implements Hash & Eq
getOrDefault = \d, key ->
    elem = Dict.get d key
    when elem is
        Ok v -> v
        Err _ -> 0

transposeList : List (Result ok err) -> Result (List ok) err
transposeList = \list ->
    when list is
        [Ok first, .. as tail] ->
            rest = transposeList? tail
            Ok (List.concat [first] rest)

        [Err first, ..] -> Err first
        [] -> Ok []
