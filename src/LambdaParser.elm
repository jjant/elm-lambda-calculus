module LambdaParser exposing (parseExpr, parse)

import Parser exposing (..)
import Lambda exposing (Expr(..), Lit(..))
import Set


parens : Parser a -> Parser a
parens p =
    succeed identity
        |. symbol "("
        |= p
        |. symbol ")"


bool : Parser Bool
bool =
    oneOf
        [ succeed True
            |. keyword "True"
        , succeed False
            |. keyword "False"
        ]


identifier : Parser String
identifier =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlpha
        , reserved = Set.empty
        }


lit : Parser Expr
lit =
    oneOf
        [ Parser.map (Lit << LInt) int
        , Parser.map (Lit << LBool) bool
        ]


var : Parser Expr
var =
    Parser.map Var identifier


args : Parser (List String)
args =
    succeed (\a b -> a :: b)
        |= identifier
        |. spaces
        |= loop [] argsHelp


argsHelp : List String -> Parser (Step (List String) (List String))
argsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= identifier
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


lambda : Parser Expr
lambda =
    succeed (\pargs body -> List.foldr Lam body pargs)
        |. symbol "\\"
        |= args
        |. symbol "."
        |= lazy (\_ -> expr)


term : Parser Expr
term =
    oneOf
        [ lazy (\_ -> parens expr)
        , var
        , lit
        , lambda
        ]


expr : Parser Expr
expr =
    succeed
        (\t ts ->
            case List.reverse ts of
                [] ->
                    List.foldl App t []

                x :: xs ->
                    List.foldl App x (xs ++ [ t ])
        )
        |= term
        |. spaces
        |= loop [] exprHelp


exprHelp : List Expr -> Parser (Step (List Expr) (List Expr))
exprHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= term
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


contents : Parser a -> Parser a
contents p =
    succeed identity
        |= p
        |. end


parseExpr : String -> Result String Expr
parseExpr code =
    case Parser.run (contents expr) code of
        Err _ ->
            Err "Error parsing expression"

        Ok a ->
            Ok a


parse c =
    Parser.run (contents expr) c
