{-# OPTIONS_GHC -Wno-name-shadowing #-}
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe
import Text.XML.HaXml.Types

data ContextFile = ContextFile{
    constants :: [Constant],
    axioms :: [Axiom]
} deriving Show

newtype Constant = Constant {identifierConst :: String} deriving Show

data Axiom = Axiom {
    labelAx :: String,
    predicateAx :: String 
    } deriving Show

main :: IO () 
main = do 
    stds <- fReadXml "testContext.xml"::IO ContextFile
    print stds
    fWriteXml "testContext-out.xml" stds

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
        where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Constant where 
    toHType (Constant i) = Defined "Constant" [] [Constr "Constant" [] [toHType i]]

instance HTypeable Axiom where 
    toHType ax = 
        let (Axiom l p) = ax in Defined "Axiom" [] [Constr "Axiom" [] [toHType l, toHType p]]

instance HTypeable ContextFile where
    toHType (ContextFile cs axs) = Defined "ContextFile" [] [Constr "ContextFile" [] [toHType cs, toHType axs]]


instance XmlContent ContextFile where 
    parseContents = inElement "org.eventb.core.contextFile" (ContextFile <$> parseConstant <*> parseAxiom)
        
    toContents v@(ContextFile co ax) = 
        [mkElemC (showConstr 0 $ toHType v) (toContents co <> toContents ax)]

instance XmlContent Constant where 
    parseContents = do 
        e <- element ["org.eventb.core.constant"]
        interior e (Constant <$> parseIdentifier e) 
        where 
            parseIdentifier i = return $ fromMaybe "unknow" $ attrToText "org.eventb.core.identifier" $ attrs i
    
    toContents v@(Constant i) = 
        [mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "org.eventb.core.identifier" i] []]

instance XmlContent Axiom where 
    parseContents = do 
        e <- element ["org.eventb.core.axiom"]
        interior e (Axiom <$> parseLabel e <*> parsePredicate e)
        where 
            parseLabel l = return $ fromMaybe "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicate p = return $ fromMaybe "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Axiom l p) = 
        [mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "org.eventb.core.label" l , mkAttr "org.eventb.core.predicate" p] []]

parseConstant :: XMLParser [Constant]
parseConstant = many parseContents

parseAxiom :: XMLParser [Axiom]
parseAxiom = many parseContents