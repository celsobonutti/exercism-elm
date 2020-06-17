module RunLengthEncoding exposing (decode, encode)


encode : String -> String
encode string =
    string
        |> String.toList
        |> List.foldl encodeChar []
        |> List.foldl stringifyTuples ""


decode : String -> String
decode string =
    string
    |> String.toList
    |> decodeChar "" Nothing

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


decodeChar : String -> Maybe String -> List Char -> String
decodeChar currentString lastNumber characters =
    let
        parsedLastNumber =
            case lastNumber of
                Nothing ->
                    0

                Just value ->
                    Maybe.withDefault 0 (String.toInt value)

        ( nextString, nextNumber ) =
            case characters of
                [] ->
                    ( currentString, Nothing )

                head :: _ ->
                    case ( lastNumber, Char.isDigit head ) of
                        ( Nothing, True ) ->
                            ( currentString, Just (String.fromChar head) )

                        ( Nothing, False ) ->
                            ( currentString ++ String.fromChar head , Nothing )

                        ( Just value, True ) ->
                            ( currentString, Just (value ++ String.fromChar head) )

                        ( Just _, False ) ->
                            ( currentString ++ String.repeat parsedLastNumber (String.fromChar head) , Nothing )
    in
    case characters of
        [] ->
            nextString

        _ :: tail ->
            decodeChar nextString nextNumber tail
