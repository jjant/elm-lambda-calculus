module Lambda exposing (Expr(..), Lit(..))


type Expr
    = Lit Lit
    | Var String
    | Lam String Expr
    | App Expr Expr


type Lit
    = LInt Int
    | LBool Bool
