module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        trimmedRemark = String.trim remark
        letters = String.filter Char.isAlpha remark
        isYelling = String.any Char.isAlpha remark && String.all Char.isUpper letters
        isQuestion = String.endsWith "?" trimmedRemark
    in
        case (isYelling, isQuestion) of
            (True, True) ->
                "Calm down, I know what I'm doing!"
        
            (True, False) ->
                "Whoa, chill out!"

            (False, True) ->
                "Sure."

            (False, False) ->
                if trimmedRemark |> String.isEmpty then
                    "Fine. Be that way!"
                else
                    "Whatever."