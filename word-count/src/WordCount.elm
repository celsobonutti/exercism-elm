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
    Dict.insert lowercaseWord
        (getWordCount lowercaseWord wordDict + 1)
        wordDict


getWordCount : String -> Dict String Int -> Int
getWordCount word wordDict =
    Maybe.withDefault 0 <| Dict.get word wordDict


removeApostrophes : String -> String
removeApostrophes word =
    if String.startsWith "'" word then
        removeApostrophes (String.dropLeft 1 word)

    else if String.endsWith "'" word then
        removeApostrophes (String.dropRight 1 word)

    else
        word
