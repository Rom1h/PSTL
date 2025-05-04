{-# LANGUAGE OverloadedStrings #-}

module LeanMachineAst.ContextToLean where

import RodinAst.ContextRodinAst (ContextFile(..), Constant(..), Axiom(..))
import Data.Text (Text, unpack,pack)

-- Récupère la partie après le premier underscore
getName :: Text -> Text
getName t =
    case unpack t of
        [] -> ""
        (x:xs) -> if x == '_' then pack xs else getName (pack xs)

-- Récupère la partie avant le premier underscore
getFlag :: Text -> Text
getFlag t =
    case unpack t of
        [] -> ""
        (x:xs) -> if x == '_' then "" else x `cons` getFlag (pack xs)
    where cons c s = pack (c : unpack s)

showType :: Constant -> [Axiom] -> String
showType _ [] = ""
showType (Constant ident) (Axiom l p : xs) =
    if ident == getName l && getFlag l == "type"
        then show (Axiom l p)
        else showType (Constant ident) xs

showProp :: Constant -> [Axiom] -> String
showProp _ [] = ""
showProp (Constant ident) (Axiom l p : xs) =
    if ident == getName l && getFlag l == "prop"
        then show (Axiom l p) ++ "\n" ++ showProp (Constant ident) xs
        else showProp (Constant ident) xs

instance Show ContextFile where
    show (ContextFile _ c a) =
        "import LeanMachines.Event.Basic\n" ++
        "import LeanMachines.Event.Ordinary\n" ++
        "import LeanMachines.Event.Convergent\n" ++
        "import LeanMachines.NonDet.Ordinary\n\n" ++
        "-- CONTEXT\nstructure BoundedCtx where\n" ++
        showConstants ++ showAxiom
      where
        showConstants = "-- CONSTANTS\n" ++ foldr (\e acc -> show e ++ "\n" ++ showType e a ++ "\n" ++ acc) "" c ++ "\n"
        showAxiom = "-- AXIOMS\n" ++ foldr (\e acc -> showProp e a ++ "\n" ++ acc) "" c ++ "\n"

instance Show Constant where
    show (Constant ident) =
        unpack ident ++ ": Nat"

instance Show Axiom where
    show (Axiom l p) =
        unpack l ++ " : " ++ unpack p
