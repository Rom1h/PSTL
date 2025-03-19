module LeanMachineAst.DataExpr where
import Data.Text

data Expr
    = Eq Expr Expr     -- expr1 = expr2
    | Inf Expr Expr    -- expr1 < expr2
    | InfEq Expr Expr  -- expr1 <= expr2
    | Sup Expr Expr    -- expr1 > expr2
    | SupEq Expr Expr  -- expr1 >= expr2
    | Impliq Expr Expr -- expr1 \=> expr2 
    | Add Expr Expr    -- expr1 + expr2
    | Sub Expr Expr    -- expr1 - expr2
    | Cons Text        -- Constante/Variable
    deriving (Show, Eq)

data CType = Nul deriving (Show)

--
textToExpr :: Text -> Expr
textToExpr = undefined

exprToText :: Expr -> Text
exprToText e = undefined
textToType :: Text -> CType
textToType = undefined

cTypeToText :: CType-> Text
cTypeToText cT = undefined

