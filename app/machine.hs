import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe
import Text.XML.HaXml.Types
data MachineInfo = MachineInfo {variables ::[Variable],invariants :: [Invariant],variants::[Variant],events::[Event]} deriving Show
data Variable = Variable {name::String} deriving Show
data Invariant = Invariant {labelInv::String,predicateInv::String} deriving Show
data Variant = Variant {labelVar :: String , expr :: String} deriving Show
data Event = Event {convergent :: Bool,labelEvent :: String,gards ::[Garde]} deriving Show
data Garde = Garde {labelGarde :: String ,predicateGarde :: String} deriving Show



main :: IO ()
main = do
	stds <- fReadXml "testMachine.xml"::IO MachineInfo
	print stds
        fWriteXml "testMachine-out.xml" stds

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
        where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Garde where
    toHType (Garde l p) = Defined "Garde" [] [Constr "Garde" [] [toHType l, toHType p]]

instance HTypeable Event where
    toHType (Event c l g) =  Defined "Event" [] [Constr "Event" [] [toHType c,toHType l, toHType g]]

instance HTypeable Invariant where
    toHType (Invariant  l p) = Defined "Invariant" [] [Constr "Invariant" [] [toHType l, toHType p]]  

instance HTypeable Variant where
    toHType (Variant l e) = Defined "Variant" [] [Constr "Variant" [] [toHType l,toHType e]]

instance HTypeable Variable where
    toHType (Variable s) = Defined "Variable" [] [Constr "Variable" [] [toHType s]]

instance HTypeable MachineInfo where
    toHType (MachineInfo var inv vn ev) = Defined "MachineInfo" [] [Constr "MachineInfo" [] [toHType var,toHType inv, toHType vn, toHType ev]]

instance XmlContent Garde where 
    parseContents = do 
        e <- element ["org.eventb.core.guard"]
        (Garde <$> parseLabel e <*> parsePredicat e)
        where
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Garde l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]

instance XmlContent Event where
    parseContents = do
        e <- element ["org.eventb.core.event"]
        interior e (Event <$> parseConverge e <*> parseLabel e <*> parseGarde)
        where
            parseConverge e = return $
                case attrToText "org.eventb.core.convergence" $ attrs e of
                    Just "1" -> True
                    _ -> False
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
    

    toContents v@(Event c l n)=
        [mkElemC (showConstr 0 $ toHType v) ([mkElemC "org.eventb.core.convergence" $ toText $ show c, mkElemC "org.eventb.core.label" $ toText l ] ++ (concatMap toContents n))  ]

instance XmlContent Variant where 
    parseContents = do 
        e <- element ["org.eventb.core.variant"]
        (Variant <$> parseLabel e <*> parsePredicat e)
        where
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Variant l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]

instance XmlContent Invariant where
    parseContents = do 
        e <- element ["org.eventb.core.invariant"]
        (Invariant <$>  parseLabel e <*> parsePredicat e)
        where 
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p
    
    toContents v@(Invariant l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]
        --[mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "name" n, mkAttr "org.eventb.core.label" l, mkAttr "org.eventb.core.predicate" p]]
        
instance XmlContent Variable where
    parseContents = do
        e<- element ["org.eventb.core.variable"]
        (Variable <$> parseName e)
        where
            parseName e = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.identifier" $ attrs e
    
    toContents v@(Variable n) = 
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "name" $ toText n] ]

instance XmlContent MachineInfo where
    parseContents = inElement "org.eventb.core.machineFile"  (MachineInfo <$> parseVariable <*> parseInvariant <*> parseVariant <*> parseEvent)


        --parseContents = inElement "Students" (Students <$> parseContents)
    
    {-toContents v@(MachineInfo a b c d) =
        [mkElemC (showConstr 0 $ toHType v) ((concatMap toContents a)++(concatMap toContents b)++(concatMap toContents c)++(concatMap toContents d)) ]
    -}
parseVariable :: XMLParser [Variable]
parseVariable =  many parseContents

parseInvariant :: XMLParser [Invariant]
parseInvariant  = many parseContents


parseVariant :: XMLParser [Variant]
parseVariant = many parseContents

parseEvent :: XMLParser [Event]
parseEvent =  many parseContents

parseGarde :: XMLParser [Garde]
parseGarde = many  parseContents