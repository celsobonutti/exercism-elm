module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    candidates
    |> List.filter (isAnagram word)

isAnagram : String -> String -> Bool
isAnagram word candidate =
    let
        lowerWord = word |> String.toLower
        lowerCandidate = candidate |> String.toLower
        orderedLowerWord = lowerWord |> String.toList |> List.sort
        orderedLowerCandidate = lowerCandidate |> String.toList |> List.sort
    in
        (lowerWord /= lowerCandidate && orderedLowerWord == orderedLowerCandidate)