module MatchingBrackets exposing (checkNextChar, isPaired)


isPaired : String -> Bool
isPaired =
    String.toList
        >> checkNextChar []


checkNextChar : List Char -> List Char -> Bool
checkNextChar openBrackets remainingCharacters =
    case remainingCharacters of
        [] ->
            List.isEmpty openBrackets

        head :: tail ->
            case head of
                '(' ->
                    checkNextChar (head :: openBrackets) tail

                '{' ->
                    checkNextChar (head :: openBrackets) tail

                '[' ->
                    checkNextChar (head :: openBrackets) tail

                ')' ->
                    case openBrackets of
                        '(' :: remainingBrackets ->
                            checkNextChar remainingBrackets tail

                        _ ->
                            False

                ']' ->
                    case openBrackets of
                        '[' :: remainingBrackets ->
                            checkNextChar remainingBrackets tail

                        _ ->
                            False

                '}' ->
                    case openBrackets of
                        '{' :: remainingBrackets ->
                            checkNextChar remainingBrackets tail

                        _ ->
                            False

                _ ->
                    checkNextChar openBrackets tail
