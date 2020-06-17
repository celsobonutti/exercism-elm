module TwelveDays exposing (recite)

import Array exposing (Array)


recite : Int -> Int -> List String
recite start stop =
    List.range start stop
        |> List.map verse


verse : Int -> String
verse number =
    let
        day =
            case Array.get (number - 1) days of
                Nothing ->
                    "Something went wrong"

                Just value ->
                    value
    in
    "On the " ++ day ++ " day of Christmas my true love gave to me:" ++ gifts number


gifts : Int -> String
gifts verseNumber =
    List.take verseNumber presents
        |> List.foldl (++) ""


presents : List String
presents =
    [ " a Partridge in a Pear Tree."
    , " two Turtle Doves, and"
    , " three French Hens,"
    , " four Calling Birds,"
    , " five Gold Rings,"
    , " six Geese-a-Laying,"
    , " seven Swans-a-Swimming,"
    , " eight Maids-a-Milking,"
    , " nine Ladies Dancing,"
    , " ten Lords-a-Leaping,"
    , " eleven Pipers Piping,"
    , " twelve Drummers Drumming,"
    ]


days : Array String
days =
    Array.fromList
        [ "first"
        , "second"
        , "third"
        , "fourth"
        , "fifth"
        , "sixth"
        , "seventh"
        , "eighth"
        , "ninth"
        , "tenth"
        , "eleventh"
        , "twelfth"
        ]
