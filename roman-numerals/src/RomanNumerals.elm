module RomanNumerals exposing (toRoman)


conversionTable : List ( Int, String )
conversionTable =
    [ ( 1000, "M" )
    , ( 900, "CM" )
    , ( 500, "D" )
    , ( 400, "CD" )
    , ( 100, "C" )
    , ( 90, "XC" )
    , ( 50, "L" )
    , ( 40, "XL" )
    , ( 10, "X" )
    , ( 9, "IX" )
    , ( 5, "V" )
    , ( 4, "IV" )
    , ( 1, "I" )
    ]


toRoman : Int -> String
toRoman number =
    case closestRoman number of
        Just ( arabic, roman ) ->
            roman ++ toRoman (number - arabic)

        Nothing ->
            ""


closestRoman : Int -> Maybe ( Int, String )
closestRoman number =
    conversionTable
        |> List.filter (bigger number)
        |> List.head


bigger : Int -> ( Int, String ) -> Bool
bigger number ( value, _ ) =
    number >= value
