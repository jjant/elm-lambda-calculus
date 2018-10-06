module Compiler exposing (..)

import Lambda exposing (Expr(..), Lit(..))


compile : Expr -> String
compile e =
    "(" ++ toJs 1 e ++ ")"


toJs : Int -> Expr -> String
toJs indent expr =
    case expr of
        Lit (LInt i) ->
            String.fromInt i

        Lit (LBool b) ->
            bool b

        Var string ->
            string

        Lam arg expr1 ->
            function indent arg (toJs (indent + 1) expr1)

        App expr1 expr2 ->
            "(" ++ toJs indent expr1 ++ ")" ++ "(" ++ toJs indent expr2 ++ ")"


function : Int -> String -> String -> String
function indent arg body =
    let
        tabs =
            String.repeat indent "\t"

        buildArgs a =
            " (" ++ a ++ ") "

        buildBody b =
            "{" ++ "\n" ++ tabs ++ "\t" ++ "return " ++ body ++ ";" ++ "\n" ++ tabs ++ "}"
    in
        "function " ++ buildArgs arg ++ buildBody body


bool : Bool -> String
bool b =
    case b of
        True ->
            "true"

        False ->
            "false"
