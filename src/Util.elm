module Util
    exposing
        ( takeWhileOneMore
        , iterateWhile
        , boolToString
        , takeLast
        )


takeWhileOneMore : (a -> Bool) -> List a -> List a
takeWhileOneMore p =
    List.foldr
        (\a b ->
            if p a then
                a :: b
            else
                [ a ]
        )
        []


iterateWhile : (a -> Bool) -> (a -> a) -> a -> List a
iterateWhile pred f a =
    if pred a then
        a :: iterateWhile pred f (f a)
    else
        [ a ]


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        False ->
            "False"


takeLast : Int -> List a -> List a
takeLast n l =
    l
        |> List.reverse
        |> List.take n
        |> List.reverse
