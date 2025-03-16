module ContextToLean where

import Context (ContextFile(..), Constant(..), Axiom(..))



getName :: String -> String
getName [] = []
getName (x:xs)
    | x == '_'  = xs
    | otherwise = getName xs

getFlag :: String -> String
getFlag [] = []
getFlag (x:xs)
    | x == '_'  = []
    | otherwise = x : getFlag xs

showType :: Constant -> [Axiom] -> String
showType _ [] = ""
showType (Constant id) (Axiom l p : xs) =
  if id == getName l && getFlag l == "type"
    then show (Axiom l p)
    else showType (Constant id) xs

showProp :: Constant -> [Axiom] ->String
showProp _ [] = ""
showProp (Constant id) (Axiom l p : xs) =
    if id == getName l && getFlag l == "prop"
    then show (Axiom l p) ++"\n" ++ (showProp (Constant id) xs)
    else showProp (Constant id) xs

instance Show ContextFile where
    show (ContextFile c a) = 
        "import LeanMachines.Event.Basic\n" ++
        "import LeanMachines.Event.Ordinary\n" ++
        "import LeanMachines.Event.Convergent\n" ++
        "import LeanMachines.NonDet.Ordinary\n\n"++
        "-- CONTEXT\nstructure BoundedCtx where\n" ++
        showConstants ++ showAxiom
        where
        showConstants = "-- CONSTANTS\n" ++ foldr (\e acc -> show e ++"\n" ++showType e a ++"\n"++ acc) "" c++"\n"
        showAxiom ="-- AXIOMS\n" ++ foldr (\e acc -> (showProp e a ) ++"\n"++ acc) "" c ++ "\n"

instance Show Constant where
    show (Constant id) =
        id ++ ": Nat"


instance Show Axiom where
    show (Axiom l p) =
        l ++ " : "++ p

