module Acronym exposing (abbreviate)

abbreviate : String -> String
abbreviate phrase =
    phrase
    |> String.toUpper
    |> String.replace "-" " "
    |> String.words
    |> List.map (String.left 1)
    |> List.foldr (++) ""