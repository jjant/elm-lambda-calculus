module Pretty exposing (ppexpr)

import Lambda exposing (Expr(..), Lit(..))
import Util exposing (boolToString)


viewVars : Expr -> List String
viewVars expr =
    case expr of
        Lam x a ->
            x :: viewVars a

        _ ->
            []


viewBody : Expr -> Expr
viewBody expr =
    case expr of
        Lam _ a ->
            viewBody a

        x ->
            x


parensIf : Bool -> String -> String
parensIf b =
    case b of
        True ->
            parens

        False ->
            identity


parens : String -> String
parens s =
    "(" ++ s ++ ")"


cspace : String -> String -> String
cspace s2 s1 =
    s1 ++ " " ++ s2


ppr : Int -> Expr -> String
ppr p e =
    case e of
        Lit (LInt a) ->
            String.fromInt a

        Lit (LBool a) ->
            boolToString a

        Var x ->
            x

        App f g ->
            (parensIf (p > 0) (ppr (p + 1) f)) |> cspace (ppr p g)

        Lam _ _ ->
            parens (("\\" ++ (String.join " " (viewVars e)) ++ ".") ++ (ppr (p + 1) (viewBody e)))


ppexpr : Expr -> String
ppexpr =
    ppr 0
