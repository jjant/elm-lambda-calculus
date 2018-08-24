module CmdLine exposing (..)

import Eval exposing (getSteps)
import Lambda exposing (Expr(..), Lit(..))
import LambdaParser exposing (parseExpr)
import Pretty exposing (ppexpr)


prettySteps : Expr -> List String
prettySteps e =
    List.map ppexpr (getSteps e)


runEval : Expr -> String
runEval e =
    String.join "\n" (prettySteps e)


process : String -> List String
process line =
    if String.isEmpty line then
        []
    else
        case parseExpr line of
            Err err ->
                [ err ]

            Ok expr ->
                prettySteps expr


parseAndSteps : String -> List Expr
parseAndSteps line =
    parseExpr line
        |> Result.map getSteps
        |> Result.withDefault []
