module Pangram exposing (isPangram)

import Set


isPangram : String -> Bool
isPangram sentence =
    (sentence
        |> String.filter Char.isAlpha
        >> String.toLower
        >> String.toList
        >> Set.fromList
        >> Set.size
    )
        == 26
