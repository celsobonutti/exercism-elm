module RunLengthEncoding exposing (decode, encode)

import Parser exposing (..)


encode : String -> String
encode string =
    string
        |> String.toList
        |> List.foldl encodeChar []
        |> List.foldl stringifyTuples ""


decode : String -> String
decode string =
    case Parser.run decodeStr string of
        Err deadEnds ->
            "Error: " ++ deadEndsToString deadEnds

        Ok val ->
            val


type alias CharGroup =
    { quantity : Maybe Int
    , character : String
    }


encodeChar : Char -> List ( Int, Char ) -> List ( Int, Char )
encodeChar currentChar charList =
    case charList of
        [] ->
            [ ( 1, currentChar ) ]

        head :: tail ->
            case head of
                ( count, lastChar ) ->
                    if lastChar == currentChar then
                        ( count + 1, lastChar ) :: tail

                    else
                        ( 1, currentChar ) :: head :: tail


stringifyTuples : ( Int, Char ) -> String -> String
stringifyTuples tuple string =
    case tuple of
        ( count, character ) ->
            if count == 1 then
                String.fromChar character ++ string

            else
                String.fromInt count ++ String.fromChar character ++ string


charParser : Parser String
charParser =
    getChompedString <| chompIf (\char -> Char.isAlpha char || char == ' ')


quantityParser : Parser (Maybe Int)
quantityParser =
    oneOf
        [ succeed Just
            |= int
        , succeed Nothing
        ]


charGroupParser : Parser CharGroup
charGroupParser =
    succeed CharGroup
        |= quantityParser
        |= charParser


decodeStr : Parser String
decodeStr =
    loop [] decodeHelp
        |> Parser.map concatGroups


decodeHelp : List CharGroup -> Parser (Step (List CharGroup) (List CharGroup))
decodeHelp groups =
    oneOf
        [ succeed (\group -> Loop (group :: groups))
            |= charGroupParser
        , succeed ()
            |> map (\_ -> Done (List.reverse groups))
        ]


concatGroups : List CharGroup -> String
concatGroups groups =
    List.foldl mergeGroup "" groups


mergeGroup : CharGroup -> String -> String
mergeGroup { quantity, character } currentStr =
    currentStr ++ String.repeat (Maybe.withDefault 1 quantity) character
