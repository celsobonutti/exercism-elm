module Allergies exposing (Allergy(..), closestPower, isAllergicTo, toList)

import Dict exposing (Dict)


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


allergens : Dict Int Allergy
allergens =
    Dict.fromList
        [ ( 1, Eggs )
        , ( 2, Peanuts )
        , ( 4, Shellfish )
        , ( 8, Strawberries )
        , ( 16, Tomatoes )
        , ( 32, Chocolate )
        , ( 64, Pollen )
        , ( 128, Cats )
        ]


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    List.member allergy (toList score)


toList : Int -> List Allergy
toList score =
    addAllergens score


addAllergens : Int -> List Allergy
addAllergens score =
    let
        power =
            closestPower score 0
    in
    if score == 0 then
        []

    else if score >= 256 then
        addAllergens (score - power)

    else
        case Dict.get power allergens of
            Just allergy ->
                allergy :: addAllergens (score - power)

            Nothing ->
                addAllergens (score - power)


closestPower : Int -> Int -> Int
closestPower number index =
    if number < 2 ^ index then
        2 ^ (index - 1)

    else
        closestPower number (index + 1)
