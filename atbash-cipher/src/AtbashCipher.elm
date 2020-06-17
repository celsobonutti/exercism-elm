module AtbashCipher exposing (decode, encode)


cipherMap : List ( Char, Char )
cipherMap =
    [ ( 'a', 'z' )
    , ( 'b', 'y' )
    , ( 'c', 'x' )
    , ( 'd', 'w' )
    , ( 'e', 'v' )
    , ( 'f', 'u' )
    , ( 'g', 't' )
    , ( 'h', 's' )
    , ( 'i', 'r' )
    , ( 'j', 'q' )
    , ( 'k', 'p' )
    , ( 'l', 'o' )
    , ( 'm', 'n' )
    ]


encode : String -> String
encode plain =
    plain
        |> String.map Char.toLower
        |> String.filter Char.isAlphaNum
        |> splitInFives
        |> String.map (encodeChar cipherMap)


decode : String -> String
decode cipher =
    cipher
        |> String.filter Char.isAlphaNum
        |> String.map (encodeChar cipherMap)


encodeChar : List ( Char, Char ) -> Char -> Char
encodeChar map character =
    case map of
        [] ->
            character

        ( left, right ) :: tail ->
            if character == left then
                right

            else if character == right then
                left

            else
                encodeChar tail character


splitInFives : String -> String
splitInFives text =
    let
        head =
            String.left 5 text

        tail =
            String.dropLeft 5 text

        isLast =
            String.length tail == 0
    in
    if isLast then
        head

    else
        head ++ " " ++ splitInFives tail
