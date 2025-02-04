import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Data.Maybe
import Text.XML.HaXml.Types

data MachineInfo = MachineInfo 
    {init :: [Event],
    seesContext :: [SeesContext], 
    invariants :: [Invariant]}

    deriving Show

newtype SeesContext = SeesContext 
    {target::String} 
    deriving Show

data Invariant = Invariant 
    {labelInv::String, 
    predicateInv::String} 
    deriving Show

data Event = Event 
    {convergent :: Bool, 
    labelEvent :: String,
    parameter :: [Parameter]} 
    deriving Show

newtype Parameter = Parameter 
    {identificateur :: String}
    deriving Show

data Action = Action 
    {assign :: String}
    deriving Show

data Garde = Garde 
    {labelGarde :: String, 
    predicateGarde :: String} 
    deriving Show

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

instance HTypeable Parameter where 
    toHType (Parameter i) = Defined "Parameter" [] [Constr "Parameter" [] [toHType i]]

instance HTypeable Action where 
    toHType (Action assign) = Defined "Action" [] [Constr "Action" [] [toHType assign]]
    
instance HTypeable Garde where
    toHType (Garde l p) = Defined "Garde" [] [Constr "Garde" [] [toHType l, toHType p]]
    
instance HTypeable Event where
    toHType i = 
        let Event c l p = i in  Defined "Event" [] [Constr "Event" [] [toHType c,toHType l,toHType p]]

instance HTypeable SeesContext where
    toHType (SeesContext target) = Defined "SeesContext" [] [Constr "SeesContext" [] [toHType target]]

instance HTypeable Invariant where
    toHType (Invariant l p) = Defined "Invariant" [] [Constr "Invariant" [] [toHType l, toHType p]]

instance HTypeable MachineInfo where
    toHType (MachineInfo init sc i) = Defined "MachineInfo" [] [Constr "MachineInfo" [] [toHType init, toHType sc, toHType i]]

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
        interior e (Action <$> parseAssignment e)
        where  
            parseAssignment as = return $ fromMaybe "unknow" $ attrToText "org.eventb.core.assignment" $ attrs as 

    toContents v@(Action i) = 
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.assignment" $ toText i] ]


{-
instance XmlContent Action where 
    parseContents = do 
        e <- element ["org.eventb.core.action"]
        interior e (Action <$> parseAssignment e <*> parseLabel e)
        where
            parseAssignment a = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.assignment" $ attrs a
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l
        
    toContents v@(Action a l) = 
        [mkElemC (showConstr 0 $ toHType v) [mkElemC "org.eventb.core.assignment" $ toText a, mkElemC "org.eventb.core.label" $ toText l] ]
-}
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
        interior e (Event <$> parseConverge e <*> parseLabel e <*> parseParameter )
        where
            parseConverge e = return $
                case attrToText "org.eventb.core.convergence" $ attrs e of
                    Just "1" -> True
                    _ -> False
            parseLabel l = return $ fromMaybe  "unknow" $ attrToText "org.eventb.core.label" $ attrs l


    toContents v@(Event c l p)=
        [mkElemC (showConstr 0 $ toHType v) ([mkElemC "org.eventb.core.convergence" $ toText $ show c, mkElemC "org.eventb.core.label" $ toText l ] <> toContents p )  ]


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

instance XmlContent MachineInfo where
    parseContents = inElement "org.eventb.core.machineFile"  (MachineInfo <$> parseEvent <*> parseSeesContext <*> parseInvariant )
        -- parseContents = inElement "Students" (Students <$> parseContents)

    toContents v@(MachineInfo init sc i) = 
        [mkElemC (showConstr 0 $ toHType v) (toContents init <> toContents sc <>toContents i)  ]
    {-toContents v@(MachineInfo a b c d) =
        [mkElemC (showConstr 0 $ toHType v) ((concatMap toContents a)++(concatMap toContents b)++(concatMap toContents c)++(concatMap toContents d)) ]
    -}

parseInvariant :: XMLParser [Invariant]
parseInvariant  = many parseContents

parseSeesContext :: XMLParser [SeesContext]
parseSeesContext = many parseContents

parseEvent :: XMLParser [Event]
parseEvent =  many parseContents

parseGarde :: XMLParser [Garde]
parseGarde = many parseContents

parseAction :: XMLParser [Action]
parseAction = many parseContents

parseParameter :: XMLParser [Parameter]
parseParameter = many parseContents