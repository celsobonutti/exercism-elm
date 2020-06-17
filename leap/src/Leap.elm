module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    let
        isDivisibleByFour = modBy 4 year == 0
        isDivisibleByOneHundred = modBy 100 year == 0
        isDivisibleByFourHundred = modBy 400 year == 0 
    in
    case (isDivisibleByFour, isDivisibleByOneHundred, isDivisibleByFourHundred) of
        (True, False, _) -> True

        (True, True, True) -> True
    
        _ -> False
            
    
