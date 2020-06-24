module Say exposing (SayError(..), say)

import Dict exposing (Dict)


type SayError
    = Negative
    | TooLarge


say : Int -> Result SayError String
say number =
    if number < 0 then
        Err Negative

    else if number > 999999999999 then
        Err TooLarge

    else if number == 0 then
        Ok "zero"

    else
        Ok (speak number)


speak : Int -> String
speak =
    String.fromInt
        >> splitIntoChunks
        >> List.reverse
        >> List.indexedMap pronounce
        >> List.foldr addToSentence ""


splitIntoChunks : String -> List String
splitIntoChunks =
    String.foldr addChar []


addToSentence : ( Int, String ) -> String -> String
addToSentence ( index, number ) currentSentence =
    let
        suffix =
            case index of
                1 ->
                    " thousand"

                2 ->
                    " million"

                3 ->
                    " billion"

                _ ->
                    ""
    in
    if String.length currentSentence == 0 then
        number ++ suffix

    else if String.length number == 0 then
        currentSentence

    else if index == 0 && not (String.contains "hundred" number) then
        currentSentence ++ " and " ++ number

    else
        currentSentence ++ " " ++ number ++ suffix


addChar : Char -> List String -> List String
addChar currentChar currentList =
    case currentList of
        head :: tail ->
            if String.length head >= 3 then
                String.fromChar currentChar :: currentList

            else
                String.cons currentChar head :: tail

        [] ->
            [ String.fromChar currentChar ]


pronounce : Int -> String -> ( Int, String )
pronounce index number =
    if number == "000" then
        ( index, "" )

    else
        ( index, getNumber number )


getNumber : String -> String
getNumber number =
    if String.length number == 3 then
        case ( String.left 1 number, String.right 2 number ) of
            ( "0", "00" ) ->
                ""

            ( "0", val ) ->
                getNumber val

            ( hundred, "00" ) ->
                getNumber hundred ++ " hundred"

            ( hundred, val ) ->
                getNumber hundred ++ " hundred and " ++ getNumber val

    else if String.length number == 2 then
        case ( String.left 1 number, String.right 1 number ) of
            ( "1", "0" ) ->
                "ten"

            ( "1", "1" ) ->
                "eleven"

            ( "1", "2" ) ->
                "twelve"

            ( "1", "3" ) ->
                "thirteen"

            ( "1", "4" ) ->
                "fourteen"

            ( "1", "5" ) ->
                "fifteen"

            ( "1", "6" ) ->
                "sixteen"

            ( "1", "7" ) ->
                "seventeen"

            ( "1", "8" ) ->
                "eighteen"

            ( "1", "9" ) ->
                "nineteen"

            ( "0", num ) ->
                getNumber num

            ( ten, "0" ) ->
                getTen ten

            ( ten, num ) ->
                getTen ten ++ "-" ++ getNumber num

    else
        Maybe.withDefault "" <| Dict.get number numberDict


getTen : String -> String
getTen ten =
    Maybe.withDefault "" <| Dict.get ten tensDict


tensDict : Dict String String
tensDict =
    Dict.fromList
        [ ( "2", "twenty" )
        , ( "3", "thirty" )
        , ( "4", "forty" )
        , ( "5", "fifty" )
        , ( "6", "sixty" )
        , ( "7", "seventy" )
        , ( "8", "eighty" )
        , ( "9", "ninety" )
        ]


numberDict : Dict String String
numberDict =
    Dict.fromList
        [ ( "1", "one" )
        , ( "2", "two" )
        , ( "3", "three" )
        , ( "4", "four" )
        , ( "5", "five" )
        , ( "6", "six" )
        , ( "7", "seven" )
        , ( "8", "eight" )
        , ( "9", "nine" )
        ]
