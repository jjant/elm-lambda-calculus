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
        |. spaces
        |= args
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> expr)


term : Parser Expr
term =
    oneOf
        [ lazy (\_ -> parens expr)
        , lazy (\_ -> var)
        , lazy (\_ -> lit)
        , lazy (\_ -> lambda)
        ]


flip f a b =
    f b a


expr : Parser Expr
expr =
    succeed (List.foldl (\a ac -> App ac a))
        -- App :: Expr -> Expr -> Expr
        |= lazy (\_ -> term)
        |. spaces
        |= loop [] exprHelp


exprHelp : List Expr -> Parser (Step (List Expr) (List Expr))
exprHelp revExprs =
    oneOf
        [ succeed (\exp -> Loop (exp :: revExprs))
            |= lazy (\_ -> term)
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revExprs))
        ]


contents : Parser a -> Parser a
contents p =
    succeed identity
        |. spaces
        |= p
        |. spaces


parseExpr : String -> Result String Expr
parseExpr code =
    case Parser.run (contents expr) code of
        Err _ ->
            Err "Error parsing expression"

        Ok a ->
            Ok a


parse c =
    Parser.run (contents expr) c
