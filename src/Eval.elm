module Eval
    exposing
        ( stepEval
        , getSteps
        )

import Lambda exposing (Expr(..))
import Util exposing (iterateWhile)


substitute : Expr -> String -> Expr -> Expr
substitute expr varName newExpression =
    case expr of
        Var x ->
            if x == varName then
                newExpression
            else
                expr

        Lam var body ->
            if var == varName then
                expr
            else
                Lam var (substitute body varName newExpression)

        App f g ->
            App
                (substitute f varName newExpression)
                (substitute g varName newExpression)

        Lit l ->
            Lit l



-- Perform one step of evaluation: (\x y . x) 3 2 -> (\y. 3) 2


stepEval : Expr -> Expr
stepEval e =
    case e of
        App (Lam var body) t ->
            substitute body var t

        App (App f g) t ->
            App (stepEval (App f g)) t

        -- Things that aren't applications, remain the same.
        _ ->
            e


isValue : Expr -> Bool
isValue e =
    case e of
        App (Lam _ _) _ ->
            False

        App (App _ _) _ ->
            False

        _ ->
            True


getSteps : Expr -> List Expr
getSteps e =
    iterateWhile (not << isValue) stepEval e
