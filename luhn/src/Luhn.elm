module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let
        noWhitespace =
            String.filter (\char -> char /= ' ') input

        sum =
            noWhitespace
                |> String.toList
                |> List.reverse
                |> List.indexedMap multiplyDoubles
                |> List.foldl (+) 0
    in
    if hasInvalidCharacters noWhitespace || String.length noWhitespace <= 1 then
        False

    else
        modBy 10 sum == 0


hasInvalidCharacters : String -> Bool
hasInvalidCharacters text =
    not
        (text
            |> String.toList
            |> List.all Char.isDigit
        )


intFromChar : Char -> Int
intFromChar char =
    Maybe.withDefault 0
        (char
            |> String.fromChar
            |> String.toInt
        )


multiplyChar : Char -> Int
multiplyChar char =
    let
        value =
            intFromChar char

        multiplied =
            2 * value
    in
    if multiplied > 9 then
        multiplied - 9

    else
        multiplied


multiplyDoubles : Int -> Char -> Int
multiplyDoubles index char =
    if modBy 2 index /= 0 then
        multiplyChar char

    else
        intFromChar char
