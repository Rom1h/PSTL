{-# LANGUAGE OverloadedStrings #-}

module RodinAst.Context (
    ContextFile(..),
    Constant(..),
    Axiom(..)
) where

{-# OPTIONS_GHC -Wno-name-shadowing #-}
import Data.Text (Text, pack, unpack)

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe
import Text.XML.HaXml.Types

-- Ajouter invariant pour nom de variable (type_var etc)
data ContextFile = ContextFile{
    constants :: [Constant],
    axioms :: [Axiom]
}

newtype Constant = Constant { identifierConst :: Text }

data Axiom = Axiom {
    labelAx :: Text,
    predicateAx :: Text 
}

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
        where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Constant where 
    toHType (Constant i) = Defined "Constant" [] [Constr "Constant" [] [toHType (unpack i)]]

instance HTypeable Axiom where 
    toHType ax = 
        let (Axiom l p) = ax in Defined "Axiom" [] [Constr "Axiom" [] [toHType (unpack l), toHType (unpack p)]]

instance HTypeable ContextFile where
    toHType (ContextFile cs axs) = Defined "ContextFile" [] [Constr "ContextFile" [] [toHType cs, toHType axs]]

instance XmlContent ContextFile where 
    parseContents = inElement "org.eventb.core.contextFile" (ContextFile <$> parseConstant <*> parseAxiom)
        
    toContents v@(ContextFile co ax) = 
        [mkElemC (showConstr 0 $ toHType v) (toContents co <> toContents ax)]

instance XmlContent Constant where 
    parseContents = do 
        e <- element ["org.eventb.core.constant"]
        interior e (Constant . pack <$> parseIdentifier e) 
        where 
            parseIdentifier i = return $ fromMaybe "unknown" $ attrToText "org.eventb.core.identifier" $ attrs i
    
    toContents v@(Constant i) = 
        [mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "org.eventb.core.identifier" (unpack i)] []]

instance XmlContent Axiom where 
    parseContents = do 
        e <- element ["org.eventb.core.axiom"]
        interior e (Axiom <$> parseLabel e <*> parsePredicate e)
        where 
            parseLabel l = return $ pack $ fromMaybe "unknown" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicate p = return $ pack $ fromMaybe "unknown" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Axiom l p) = 
        [mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "org.eventb.core.label" (unpack l), mkAttr "org.eventb.core.predicate" (unpack p)] []]

parseConstant :: XMLParser [Constant]
parseConstant = many parseContents

parseAxiom :: XMLParser [Axiom]
parseAxiom = many parseContents
