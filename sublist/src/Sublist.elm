module Sublist exposing (ListComparison(..), sublist)

type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist =
    let
        lengthA =
            List.length alist

        lengthB =
            List.length blist
    in
    if alist == blist then
        Equal

    else if lengthA == lengthB then
        Unequal

    else if lengthA > lengthB && isSublist blist alist then
        Superlist

    else if lengthA < lengthB && isSublist alist blist then
        Sublist

    else
        Unequal


isSublist : List a -> List a -> Bool
isSublist lista listb =
    let
        length =
            List.length lista
    in
    if List.take length listb == lista then
        True

    else
        case listb of
            [] ->
                False

            _ :: tail ->
                if List.length tail < length then
                    False

                else
                    isSublist lista tail
