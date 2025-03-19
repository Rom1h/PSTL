module RodinAst.Machine where

{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe
import Text.XML.HaXml.Types

data MachineInfo = MachineInfo {
    init :: [Event],
    seesContext :: [SeesContext],
    variables ::[Variable],
    invariants :: [Invariant],
    variants::[Variant],
    events::[Event]
    } 

newtype SeesContext = SeesContext
    {target::String}

data Variable = Variable {name::String} 
data Invariant = Invariant {labelInv::String,predicateInv::String} 
data Variant = Variant {labelVar :: String , expr :: String} 
data Event = Event {convergent :: Bool,labelEvent :: String,parameter :: [Parameter],gards ::[Garde],action:: [Action]} 
data Garde = Garde {labelGarde :: String ,predicateGarde :: String} deriving (Show)

newtype Parameter = Parameter 
    {identificateur :: String}
    deriving Show

data Action = Action 
    {assign :: String}
    deriving Show

attrToText :: String -> [Attribute] -> Maybe String
attrToText n as = foldl1 (<|>) attrText
        where attrText = map (fromAttrToStr n) as

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

instance HTypeable Parameter where 
    toHType (Parameter i) = Defined "Parameter" [] [Constr "Parameter" [] [toHType i]]

instance HTypeable Action where 
    toHType (Action assign) = Defined "Action" [] [Constr "Action" [] [toHType assign]]

instance HTypeable Garde where
    toHType i =
        let (Garde l p) = i in Defined "Garde" [] [Constr "Garde" [] [toHType l, toHType p]]

instance HTypeable SeesContext where
    toHType i =
        let (SeesContext target) = i in Defined "SeesContext" [] [Constr "SeesContext" [] [toHType target]]

instance HTypeable Event where
    toHType i =
        let (Event c l p g a) = i in Defined "Event" [] [Constr "Event" [] [toHType c,toHType l,toHType p,toHType a, toHType g]]

instance HTypeable Invariant where
    toHType i =
        let  (Invariant l p) = i in Defined "Invariant" [] [Constr "Invariant" [] [toHType l, toHType p]]  

instance HTypeable Variant where
    toHType i = 
        let (Variant l e) = i in Defined "Variant" [] [Constr "Variant" [] [toHType l,toHType e]]

instance HTypeable Variable where
    toHType i =
        let  (Variable s) = i in Defined "Variable" [] [Constr "Variable" [] [toHType s]]

instance HTypeable MachineInfo where
    toHType (MachineInfo init sees var inv vn ev) = Defined "MachineInfo" [] [Constr "MachineInfo" [] [toHType init,toHType sees,toHType var,toHType inv, toHType vn, toHType ev]]

instance XmlContent SeesContext where
    parseContents = do
        e <- element ["org.eventb.core.seesContext"]
        SeesContext <$> parseTarget e
        where
            parseTarget target = return $ fromMaybe "unknow" $ attrToText "org.eventb.core.target" $ attrs target

    toContents v@(SeesContext target) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.target" $ toText target] ]

instance XmlContent Garde where 
    parseContents = do 
        e <- element ["org.eventb.core.guard"]
        Garde <$> parseLabel e <*> parsePredicat e
        where
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Garde l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]

instance XmlContent Action where 
    parseContents = do 
        e <- element ["org.eventb.core.action"]
        Action <$> parseAssignment e 
        where
            parseAssignment ass = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.assignment" $ attrs ass
        
    toContents v@(Action ass) = 
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.assignment" $ toText ass] ]

instance XmlContent Parameter where 
    parseContents = do 
        e <- element ["org.eventb.core.parameter"]
        interior e (Parameter <$> parseIdentifier e)
        where  
            parseIdentifier i = return $ fromMaybe "unknow" $ attrToText "org.eventb.core.identifier" $ attrs i 
    toContents v@(Parameter i) = 
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.identifier" $ toText i] ]

instance XmlContent Event where
    parseContents = do
        e <- element ["org.eventb.core.event"]
        interior e (Event <$> parseConverge e <*> parseLabel e <*>  parseParameter <*> parseGarde <*> parseAction)
        where
            parseConverge e = return $
                case attrToText "org.eventb.core.convergence" $ attrs e of
                    Just "1" -> True
                    _ -> False
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l

    toContents v@(Event c l p n a)=
        [mkElemC (showConstr 0 $ toHType v) ([mkElemC "org.eventb.core.convergence" $ toText $ show c, mkElemC "org.eventb.core.label" $ toText l ] ++ (concatMap toContents p)++ (concatMap toContents n) ++ (concatMap toContents a))  ]

    toContents v@(Event c l p n a)=
        [mkElemC (showConstr 0 $ toHType v) ([mkElemC "org.eventb.core.convergence" $ toText $ show c, mkElemC "org.eventb.core.label" $ toText l ] <>  toContents p <> toContents n <> toContents a)  ]

instance XmlContent Variant where
    parseContents = do
        e <- element ["org.eventb.core.variant"]
        Variant <$> parseLabel e <*> parsePredicat e
        where
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Variant l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]

instance XmlContent Invariant where
    parseContents = do
        e <- element ["org.eventb.core.invariant"]
        Invariant <$>  parseLabel e <*> parsePredicat e
        where
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
            parsePredicat p = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.predicate" $ attrs p

    toContents v@(Invariant l p) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.label" $ toText l, mkElemC "org.eventb.core.predicate" $ toText p] ]
        --[mkAttrElemC (showConstr 0 $ toHType v) [mkAttr "name" n, mkAttr "org.eventb.core.label" l, mkAttr "org.eventb.core.predicate" p]]

instance XmlContent Variable where
    parseContents = do
        e<- element ["org.eventb.core.variable"]
        Variable <$> parseName e
        where
            parseName e = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.identifier" $ attrs e

    toContents v@(Variable n) =
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "name" $ toText n] ]

instance XmlContent MachineInfo where
    
    parseContents = inElement "org.eventb.core.machineFile"  (MachineInfo <$> parseEvent <*> parseSeesContext <*> parseVariable <*> parseInvariant <*> parseVariant <*> parseEvent)
    
    toContents v@(MachineInfo  e s a b c d) =
        [mkElemC (showConstr 0 $ toHType v) ((concatMap toContents e)++(concatMap toContents s)++(concatMap toContents a)++(concatMap toContents b)++(concatMap toContents c)++(concatMap toContents d)) ]
    
parseVariable :: XMLParser [Variable]
parseVariable =  many parseContents

parseInvariant :: XMLParser [Invariant]
parseInvariant  = many parseContents

parseSeesContext :: XMLParser [SeesContext]
parseSeesContext = many parseContents

parseVariant :: XMLParser [Variant]
parseVariant = many parseContents

parseEvent :: XMLParser [Event]
parseEvent =  many parseContents

parseGarde :: XMLParser [Garde]
parseGarde = many parseContents

parseAction :: XMLParser [Action]
parseAction = many parseContents

parseParameter :: XMLParser [Parameter]
parseParameter = many parseContents