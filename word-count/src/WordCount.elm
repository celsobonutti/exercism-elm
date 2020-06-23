module WordCount exposing (wordCount)

import Dict exposing (Dict)


wordCount : String -> Dict String Int
wordCount sentence =
    sentence
        |> splitIntoWords
        |> List.foldl addWord Dict.empty


splitIntoWords : String -> List String
splitIntoWords =
    String.replace "," " "
        >> String.filter (\char -> Char.isAlphaNum char || char == ' ' || char == '\'')
        >> String.words
        >> List.map removeApostrophes


addWord : String -> Dict String Int -> Dict String Int
addWord word wordDict =
    let
        lowercaseWord =
            String.toLower word
    in
    wordDict
        |> (case Dict.get lowercaseWord wordDict of
                Nothing ->
                    Dict.insert lowercaseWord 1

                Just value ->
                    Dict.insert lowercaseWord (value + 1)
           )


removeApostrophes : String -> String
removeApostrophes word =
    if String.startsWith "'" word then
        removeApostrophes (String.dropLeft 1 word)

    else if String.endsWith "'" word then
        removeApostrophes (String.dropRight 1 word)

    else
        word
