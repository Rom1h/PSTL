{-# LANGUAGE OverloadedStrings #-}

module LeanMachineAst.DataExpr where
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char


data Expr
    = Eq Expr Expr
    | NotEq Expr Expr    -- expr1 = expr2
    | Inf Expr Expr    -- expr1 < expr2
    | InfEq Expr Expr  -- expr1 <= expr2
    | Sup Expr Expr    -- expr1 > expr2
    | SupEq Expr Expr  -- expr1 >= expr2
    | Impliq Expr Expr -- expr1 \=> expr2
    | Or Expr Expr     
    | And Expr Expr 
    | Add Expr Expr    -- expr1 + expr2
    | Sub Expr Expr    -- expr1 - expr2
    | Mul Expr Expr
    | In Expr CType -- expr ∈ ℕ
    | Aff Expr Expr
    | Ens [Expr]
    | Not Expr
    | Cons Text        -- Constante/Variable
    
    deriving (Show, Eq)

data CType = Nat
            |None deriving (Show,Eq)

xmlSymbolToOp :: Text -> Text
xmlSymbolToOp =
    T.replace "&gt;" ">" .
    T.replace "&lt;" "<" .
    T.replace "&#10;" " "


tokenize :: Text -> [Text]
tokenize txt
  | T.null txt = []
  | T.head txt `elem` ("()¬+−*=≔<∧∨≠≥≤⇒>∈{}," :: String) =
      T.singleton (T.head txt) : tokenize (T.tail txt)
  | T.head txt == ' ' =
      tokenize (T.tail txt)
  | T.head txt == '\n' =
      tokenize (T.tail txt)
  | otherwise =
      let (t1, rest) = T.span (\c ->isAlphaNum c || c=='_') txt
      in t1 : tokenize rest


textToExpr :: Text -> Expr
textToExpr = fst . parseImplic . tokenize. xmlSymbolToOp



parseImplic :: [Text] ->(Expr, [Text])
parseImplic tokens =
    let (e1,rest) = parseOrOperator tokens
    in case rest of
        ( "⇒" : xs) -> let (e2, rest2) = parseImplic xs in (Impliq e1 e2, rest2)
        _ ->(e1,rest)

parseOrOperator :: [Text] ->(Expr, [Text])
parseOrOperator tokens =
    let (e1,rest) = parseAndOperator tokens
    in case rest of
        ("∨" : xs) -> let (e2, rest2) = parseOrOperator xs in (Or e1 e2, rest2)
        _ ->(e1,rest)

parseNot :: [Text] -> (Expr, [Text])
parseNot ("¬" : xs) =
    let (e1, rest) = parseNot xs
    in (Not e1, rest)
parseNot tokens = parseCompOperator tokens
parseAndOperator :: [Text] -> (Expr, [Text])
parseAndOperator tokens =
    let (e1, rest) = parseNot tokens
    in case rest of
        ("∧" : xs) -> let (e2, rest2) = parseAndOperator xs in (And e1 e2, rest2)
        _->(e1,rest)
        
parseCompOperator :: [Text] -> (Expr, [Text])
parseCompOperator tokens =
  let (e1, rest) = parseNumOperator tokens
  in case rest of
    ( ">" : xs) -> let (e2, rest2) = parseCompOperator xs in (Sup e1 e2, rest2)
    ("≥" : xs) -> let (e2, rest2) = parseCompOperator xs in (SupEq e1 e2, rest2)
    ("<" : xs) -> let (e2, rest2) = parseCompOperator xs in (Inf e1 e2, rest2)
    ("≤" : xs) -> let (e2, rest2) = parseCompOperator xs in (InfEq e1 e2, rest2)
    ("=" : xs) -> let (e2, rest2) = parseCompOperator xs in (Eq e1 e2, rest2)
    ("≔" : xs) -> let (e2, rest2) = parseCompOperator xs in (Aff e1 e2, rest2)
    ("≠" : xs) -> let (e2, rest2) = parseCompOperator xs in (NotEq e1 e2, rest2)
    ("∈" : xs) -> 
            case xs of
                (x:rest2) -> (In e1 (matchCType x), rest2)
                _ -> error "Expected a type after ∈"
    _->(e1,rest)

parseNumOperator :: [Text] -> (Expr, [Text])
parseNumOperator tokens =
  let (e1, rest) = parseVar tokens
  in case rest of
    ("+" : xs) -> let (e2, rest2) = parseNumOperator xs in (Add e1 e2, rest2)
    ("−" : xs) -> let (e2, rest2) = parseNumOperator xs in (Sub e1 e2, rest2)
    ("*" : xs) -> let (e2, rest2) = parseNumOperator xs in (Mul e1 e2, rest2)
    _->(e1,rest)

parseVar :: [Text] -> (Expr,[Text])
parseVar ("{":xs) =
    let (elements, rest) = parseSet xs
    in (Ens elements, rest)
parseVar ("(":xs) =
    let (e , rest) = parseImplic xs
    in case rest of
        (")" : xl) -> (e, xl)
        (_:x2)-> (e,x2)
parseVar (x : xs)
    | T.all (\c ->isAlphaNum c || c=='_') x = (Cons x, xs)
    | otherwise = error "Unexepted token"


parseSet :: [Text] -> ([Expr], [Text])
parseSet ("}":xs) = ([], xs)  
parseSet tokens =
    let (e1, rest1) = parseImplic tokens
    in case rest1 of
        ("," : rest2) ->
            let (es, rest3) = parseSet rest2
            in (e1 : es, rest3)
        ("}" : rest2) -> ([e1], rest2)
        _ -> error "Expected ',' or '}' in set"

{-
textToExpr :: Text -> Expr
textToExpr t = 
    let t2 = replaceSymbol t
    let splitT2 = T.splitOn " " t2
    generate_expr splitT2 where
    generate_expr [] = error "empty list"
    generate_expr [a] = getExpr a
    generate_expr x:y:z:l =
        -}
{-
concateExpr :: Text -> Expr -> Expr -> expr1
concateExpr t e1 e2
|t == pack "=" = Eq e1 e2
|t == pack "≠" = NotEq e1 e2
|t == pack "<" = Inf e1 e2
|t == pack "≤" = InfEq e1 e2
|t == pack "≥" = SupEq e1 e2
|t == pack ">" = Sup e1 e2
|t == pack "⇒" = Impliq e1 e2-}


-- A modifier pour meilleur indentation
exprToText :: Expr -> Text
exprToText (Eq e1 e2) = exprToText e1 <> "=" <> exprToText e2
exprToText (NotEq e1 e2) =  exprToText e1 <> "≠" <> exprToText e2
exprToText (Inf e1 e2) = exprToText e1 <> "<" <> exprToText e2
exprToText (InfEq e1 e2) = exprToText e1 <> "≤" <> exprToText e2
exprToText (Sup e1 e2) = exprToText e1 <> ">" <> exprToText e2
exprToText (Sup e1 e2) = exprToText e1 <> "≥" <> exprToText e2
exprToText (Impliq e1 e2) = exprToText e1 <> "⇒" <> exprToText e2
exprToText (Or e1 e2) = exprToText e1 <> "∨" <> exprToText e2
exprToText (And e1 e2) = exprToText e1 <> "∧" <> exprToText e2
exprToText (Add e1 e2) = exprToText e1 <> "+" <> exprToText e2
exprToText (Sub e1 e2) = exprToText e1 <> "−" <> exprToText e2
exprToText (Mul e1 e2) = exprToText e1 <> "*" <> exprToText e2
exprToText (In e t) = exprToText e <> "∈" <> cTypeToText t
exprToText (Aff e1 e2) = exprToText e1 <> "≔" <> exprToText e2
exprToText (Ens e) = "{" <> T.intercalate "," (map exprToText e) <> "}"
exprToText (Not e) = "¬" <> exprToText e



exprToText (Cons e) = e

textToType :: Text -> CType
textToType t =
    case T.splitOn (T.pack " ∈ ") t of 
        [_, typeT] -> matchCType typeT
        _ -> None


matchCType :: Text -> CType
matchCType t
    | t == T.pack "ℕ" = Nat
    | otherwise = None

cTypeToText :: CType-> Text
cTypeToText cT
    | cT == Nat = T.pack "Nat"
    | otherwise = T.pack "None"

