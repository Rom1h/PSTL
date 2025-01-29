import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe 
import Text.XML.HaXml.Types

data Context = Context {info :: [ContextInfo] } deriving Show

data ContextInfo = 
    Constant String 
    |Axiom String
    deriving Show
main :: IO ()
main = do
	stds <- fReadXml "testContext.xml"::IO Context
	print stds
        fWriteXml "testContext-out.xml" stds

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
       where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Context where
   toHType (Context ctx) = Defined "Context" [] [Constr "Context" [] [toHType ctx]]

instance HTypeable ContextInfo where
    toHType (Axiom s) = Defined "Axiom" [] [Constr "Axiom" [] [toHType s]]
    toHType (Constant s) =  Defined "Constant" [] [Constr "Constant" [] [toHType s]]

instance XmlContent ContextInfo where
    parseContents = do 
	     e <- element ["org.eventb.core.constant"]
             interior e (Constant <$> parseName e) 
	    where 
               parseName e = return $ fromMaybe "unknown" $ attrToText "org.eventb.core.identifier" $ attrs e

    toContents v@(Constant ss) =
        [mkAttrElemC "org.eventb.core.constant" [mkAttr "name" ss] []]
        -- [mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "gender" g, mkAttr "age" age]  ( mkElemC "name" (toText n) : toContents a)]

instance XmlContent Context where
    parseContents = inElement "org.eventb.core.contextFile" (Context <$> many parseContents)
         --parseContents = inElement "Students" (Students <$> parseContents)
    
    toContents v@(Context ss) =
       [mkElemC "org.eventb.core.contextFile" (concatMap toContents ss)]