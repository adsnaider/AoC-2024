app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import Util

import "day2.txt" as input : Str

lines = \in ->
    in
    |> Str.splitOn "\n"
    |> List.map Str.trim

parseReport : Str -> Result (List I64) _
parseReport = \row ->
    row
    |> Str.splitOn " "
    |> List.map (\elem -> elem |> Str.trim |> Str.toI64)
    |> Util.transposeList

parseInput = \in ->
    in
    |> lines
    |> List.map (\row -> parseReport row)
    |> Util.transposeList

isSafeRecord = \record ->
    (increasesBy record 3) || (decreasesBy record 3)

increasesBy = \list, increase ->
    when list is
        [] -> Bool.true
        [_] -> Bool.true
        [a, b, .. as tail] ->
            (b - a > 0 && b - a <= increase) && increasesBy (List.concat [b] tail) increase

decreasesBy = \list, increase ->
    increasesBy (List.reverse list) increase

part1 : Result U64 _
part1 =
    parsed = parseInput? input
    parsed
    |> List.countIf (\record -> isSafeRecord record)
    |> Ok

isPartiallySafeRecord = \record ->
    (partiallyIncreasesBy record 3 None) || (partiallyDecreasesBy record 3 None)

partiallyDecreasesBy = \list, increase, prev ->
    reversed = List.reverse list
    partiallyIncreasesBy reversed increase prev

within = \a, b, max ->
    b - a > 0 && b - a <= max

partiallyIncreasesBy = \list, increase, prev ->
    when list is
        [] -> Bool.true
        [_] -> Bool.true
        [_, _] -> Bool.true
        [a, b, c, .. as tail] ->
            if within a b increase then
                partiallyIncreasesBy (List.concat [b, c] tail) increase (Some a)
            else if within a c increase then
                increasesBy (List.concat [a, c] tail) increase
            else
                when prev is
                    Some old -> within old b increase && increasesBy (List.concat [b, c] tail) increase
                    None -> increasesBy (List.concat [b, c] tail) increase

part2 =
    parsed = parseInput? input
    parsed
    |> List.countIf (\record -> isPartiallySafeRecord record)
    |> Ok

run = part2
main = run |> Util.asExit

expect isPartiallySafeRecord [7, 6, 4, 2, 1]
expect isPartiallySafeRecord [1, 2, 4, 6, 7]
expect !(isPartiallySafeRecord [1, 2, 7, 8, 9])
expect !(isPartiallySafeRecord [9, 7, 6, 2, 1])
expect isPartiallySafeRecord [1, 3, 2, 4, 5]
expect isPartiallySafeRecord [8, 6, 4, 4, 1]
expect isPartiallySafeRecord [1, 4, 6, 7, 9]
expect isPartiallySafeRecord [100, 4, 6, 7, 9]
expect isPartiallySafeRecord [4, 6, 7, 9, 100]
expect isPartiallySafeRecord [4, 6, 7, 100, 9]
expect isPartiallySafeRecord [4, 100, 7, 8, 9]
expect isPartiallySafeRecord [4, 6, 100, 8, 9]
expect !(isPartiallySafeRecord [4, 6, 100, 100, 9])
expect !(isPartiallySafeRecord [4, 6, 100, 6, 9])
expect !(isPartiallySafeRecord [4, 6, 100, 9, 9])
expect isPartiallySafeRecord [100, 101, 98, 96, 94]
expect !(isPartiallySafeRecord [100, 101, 100, 96, 94])
expect !(isPartiallySafeRecord [100, 101, 100, 96, 94])
