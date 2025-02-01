import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe 
import Text.XML.HaXml.Types

data ContextInfo = ContextInfo {info :: [(Constant, Invariant)]} deriving Show -- Le couple de constant invariant créer des problèmes de lecture de notre xml
data Constant = Constant {name::String, identifier::String } deriving Show
data Invariant = Invariant {namInv::String, label::String, predicate::String} deriving Show

main :: IO ()
main = do
	stds <- fReadXml "testContext.xml"::IO ContextInfo
	print stds
        fWriteXml "testContext-out.xml" stds

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
       where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Constant where
   toHType (Constant n i) = Defined "Constant" [] [Constr "Constant" [] [toHType n, toHType i]]

instance HTypeable Invariant where 
    toHType i =
        let Invariant n l p = i in Defined "Invariant" [] [Constr "Invariant" [] [toHType n, toHType l, toHType p]]  

instance HTypeable ContextInfo where
    toHType (ContextInfo i) = Defined "ContextInfo" [] [Constr "ContextInfo" [] [toHType i]]

instance XmlContent Invariant where 
    parseContents = do 
	     e <- element ["org.eventb.core.invariant"]
             interior e (Invariant <$> parseNameInv e <*> parseLabel e <*> parsePredicat e)
        where 
            parseNameInv n = return $ fromMaybe  "unknow" $ attrToText "name" $ attrs n
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p
    
    toContents v@(Invariant n l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "name" $ toText n, mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]
        --[mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "name" n, mkAttr "org.eventb.core.label" l, mkAttr "org.eventb.core.predicate" p]]
        

instance XmlContent Constant where
    parseContents = do 
	     e <- element ["org.eventb.core.constant"]
             interior e (Constant <$> parseName e <*> parseIdentifier e) 
	    where 
               parseName n = return $ fromMaybe "unknown" $ attrToText "name" $ attrs n
               parseIdentifier i = return $ fromMaybe "unknown" $ attrToText "org.eventb.core.identifier" $ attrs i

    toContents v@(Constant ss i ) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "name" $ toText ss, mkElemC "org.eventb.identifier" $ toText i]]
        -- [mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "gender" g, mkAttr "age" age]  ( mkElemC "name" (toText n) : toContents a)]

instance XmlContent ContextInfo where
    parseContents = inElement "org.eventb.core.contextFile"(ContextInfo <$> parseContents)

         --parseContents = inElement "Students" (Students <$> parseContents)
    
    toContents v@(ContextInfo i) =
       [mkElemC (showConstr 0 $ toHType v) (concatMap toContents i) ]