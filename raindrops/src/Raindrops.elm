module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    let 
        (_, message) = 
            (number, "")
            |>appendIfHasFactor 3 "Pling"
            |>appendIfHasFactor 5 "Plang"
            |>appendIfHasFactor 7 "Plong"
    in
    if String.isEmpty message then
        String.fromInt number
    else
        message

appendIfHasFactor : Int -> String -> (Int, String) -> (Int, String)
appendIfHasFactor factor text (number, currentString) =
    if modBy factor number == 0 then
        (number, currentString ++ text)
    else
        (number, currentString)
