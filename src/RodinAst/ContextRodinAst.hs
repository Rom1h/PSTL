{-# LANGUAGE OverloadedStrings #-}

module RodinAst.ContextRodinAst where

import qualified Data.Text as T
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Data.Maybe (fromMaybe)

data ContextFile = ContextFile {
    constants :: [Constant],
    axioms :: [Axiom]
} deriving (Show)

newtype Constant = Constant { identifierConst :: T.Text }deriving (Show)

data Axiom = Axiom {
    labelAx :: T.Text,
    predicateAx :: T.Text 
}deriving (Show)

-- Génération d'une map de balises par nom: permet de stocker tout les balises de meme nom dans une liste.
generateBalise :: [Content i] -> Map.Map String [Content i] -> Map.Map String [Content i]
generateBalise [] m = m
generateBalise (CElem (Elem (N bName) at l) t : xs) m =
    let currentList = Map.findWithDefault [] bName m
        updatedMap = Map.insert bName (CElem (Elem (N bName) at l) t: currentList) m
    in generateBalise xs updatedMap
generateBalise (_ : xs) m = generateBalise xs m

attValueToText :: AttValue -> T.Text
attValueToText (AttValue parts) = T.concat $ map extract parts
  where
    extract (Left str) = T.pack str
    extract (Right (RefEntity ref)) = T.pack ("&" ++ ref ++ ";") -- Pour récuperer les symbole &gt; etc..

-- Fonction pour extraire la valeur d'un attribut donné
getValue :: String -> [Attribute] -> T.Text
getValue _ [] = ""
getValue n ((N aName, val):xs)
    | aName == n = attValueToText val
    | otherwise  = getValue n xs

-- Générer les constantes depuis les balises
generateConstant :: [Content i ] -> [Constant]
generateConstant cons =
    map (\(CElem (Elem _ atts _) _) -> Constant (getValue "org.eventb.core.identifier" atts)) cons

-- Générer les axiomes depuis les balises
generateAxiom :: [Content i] -> [Axiom]
generateAxiom axioms =
    map (\(CElem (Elem _ atts _) _) ->
            Axiom (getValue "org.eventb.core.label" atts)
                   (getValue "org.eventb.core.predicate" atts)
        ) axioms

generateContextRodinAst :: Map.Map String [Content i] -> ContextFile
generateContextRodinAst m =
    let constantsBalise = Map.findWithDefault [] "org.eventb.core.constant" m
        axiomsBalise    = Map.findWithDefault [] "org.eventb.core.axiom" m
    in ContextFile (generateConstant constantsBalise) (generateAxiom axiomsBalise)