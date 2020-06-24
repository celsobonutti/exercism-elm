module LargestSeriesProduct exposing (largestProduct)


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if isInvalidInput length series then
        Nothing

    else if length == 0 then
        Just 1

    else
        splitInSeries length series
            |> List.map getProduct
            |> List.maximum


isInvalidInput : Int -> String -> Bool
isInvalidInput length series =
    length
        > String.length series
        || length
        < 0
        || String.any (\char -> not <| Char.isDigit char) series


splitInSeries : Int -> String -> List String
splitInSeries length series =
    if String.length series > length then
        String.left length series :: splitInSeries length (String.dropLeft 1 series)

    else
        [ series ]


getProduct : String -> Int
getProduct =
    String.split ""
        >> List.map (\el -> Maybe.withDefault 1 <| String.toInt el)
        >> List.product
