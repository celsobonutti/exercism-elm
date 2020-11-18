module Triangle exposing (rows)

import Array exposing (Array)


rows : Int -> List (List Int)
rows n =
    makeList n []
        |> List.reverse


makeList : Int -> List (List Int) -> List (List Int)
makeList index currentList =
    if index <= 0 then
        currentList

    else
        let
            newList =
                case currentList of
                    [] ->
                        [ [ 1 ] ]

                    latestRow :: _ ->
                        makeNextRow latestRow :: currentList
        in
        makeList (index - 1) newList


makeNextRow : List Int -> List Int
makeNextRow latestRow =
    let
        newRow =
            1 :: latestRow

        latestArray =
            Array.fromList latestRow
    in
    List.indexedMap (calculateElement latestArray) newRow


calculateElement : Array Int -> Int -> Int -> Int
calculateElement latestRow index _ =
    Maybe.withDefault 0
        (Array.get
            (index - 1)
            latestRow
        )
        + Maybe.withDefault 0
            (Array.get
                index
                latestRow
            )
