module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    List.range 1 (limit - 1)
        |> List.filter (isDivisible divisors)
        |> List.foldl (+) 0


isDivisible : List Int -> Int -> Bool
isDivisible divisors number =
    List.any (\divisor -> modBy divisor number == 0) divisors
