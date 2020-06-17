module Series exposing (slices)

type Validation
    = Valid
    | Invalid String


slices : Int -> String -> Result String (List (List Int))
slices size input =
    case validate size input of
        Invalid error ->
            Err error

        Valid ->
            Ok
                (input
                    |> String.toList
                    |> List.map String.fromChar
                    |> List.map (\number -> Maybe.withDefault 0 (String.toInt number))
                    |> getSlices size
                )


getSlices : Int -> List Int -> List (List Int)
getSlices size list =
    case list of
        [] ->
            []

        _ :: tail ->
            if size == List.length list then
                [ list ]

            else
                List.take size list :: getSlices size tail


validate : Int -> String -> Validation
validate size input =
    if String.length input == 0 then
        Invalid "series cannot be empty"

    else if size > String.length input then
        Invalid "slice length cannot be greater than series length"

    else if size == 0 then
        Invalid "slice length cannot be zero"

    else if size < 0 then
        Invalid "slice length cannot be negative"

    else
        Valid
