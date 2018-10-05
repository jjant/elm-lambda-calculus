module Compiler exposing (..)

import Lambda exposing (Expr(..), Lit(..))


compile : Expr -> String
compile e =
    "(" ++ toJs e ++ ")"


toJs : Expr -> String
toJs expr =
    case expr of
        Lit (LInt i) ->
            String.fromInt i

        Lit (LBool b) ->
            bool b

        Var string ->
            string

        Lam arg expr1 ->
            function arg (toJs expr1)

        App expr1 expr2 ->
            "(" ++ toJs expr1 ++ ")" ++ "(" ++ toJs expr2 ++ ")"


function : String -> String -> String
function arg body =
    let
        buildArgs a =
            " (" ++ a ++ ") "

        buildBody b =
            "{" ++ "\n\treturn " ++ body ++ ";" ++ "\n}"
    in
        "function " ++ buildArgs arg ++ buildBody body


bool : Bool -> String
bool b =
    case b of
        True ->
            "true"

        False ->
            "false"
