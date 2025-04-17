{-# LANGUAGE OverloadedStrings #-}

module RodinAst.MachineRodinAst where

import qualified Data.Text as T
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Data.Maybe (fromMaybe)

data MachineInfo = MachineInfo {
    init :: [Event],
    seesContext :: [SeesContext],
    variables ::[Variable],
    invariants :: [Invariant],
    variants::[Variant],
    refines::[Refines],
    events::[Event]
    } 

newtype Refines = Refines {targetref::T.Text}

newtype SeesContext = SeesContext
    {target::T.Text}

data Variable = Variable {name::T.Text} 
data Invariant = Invariant {labelInv::T.Text,predicateInv::T.Text} 
data Variant = Variant {labelVar :: T.Text , expr :: T.Text} 
data Event = Event {convergent :: Bool,labelEvent :: T.Text,parameter :: [Parameter],gards ::[Garde],action:: [Action], refinesEv::[RefinesEvent]} 
data Garde = Garde {labelGarde :: T.Text ,predicateGarde :: T.Text} deriving Show

newtype RefinesEvent = RefinesEvent
    {targetRefEv::T.Text}
    deriving Show

newtype Parameter = Parameter 
    {identificateur :: T.Text}
    deriving Show

data Action = Action 
    {assign :: T.Text}
    deriving Show

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

generateAction :: [Content i] -> [Action]
generateAction action = 
    map(\(CElem (Elem _ atts _) _) -> 
            Action (getValue "org.eventb.core.assignment" atts) {- Pas besoin de stocker le label -}
        ) action

generateRefinesEvent :: [Content i] -> [RefinesEvent]
generateRefinesEvent refinesEv = 
    map(\(CElem (Elem _ atts _) _) -> 
            RefinesEvent (getValue "org.eventb.core.target" atts)
        ) refinesEv

generateParameter::[Content i] -> [Parameter]
generateParameter param = 
    map(\(CElem (Elem _ atts _) _) -> 
            Parameter (getValue "org.eventb.core.identifier" atts)
        ) param

generateGarde::[Content i] -> [Garde]
generateGarde gard =
    map(\(CElem (Elem _ atts _) _) -> 
            Garde (getValue "org.eventb.core.label" atts) (getValue "org.eventb.core.predicate" atts)
        ) gard

generateVariant::[Content i] -> [Variant]
generateVariant variant = 
    map(\(CElem (Elem _ atts _) _) -> 
            Variant (getValue "org.eventb.core.label" atts) (getValue "org.eventb.core.expression" atts)
        ) variant

generateInvariant::[Content i] -> [Invariant]
generateInvariant invariant = 
    map(\(CElem (Elem _ atts _) _) -> 
            Invariant (getValue "org.eventb.core.label" atts) (getValue "org.eventb.core.predicate" atts)
        ) invariant

generateVariable::[Content i] -> [Variable]
generateVariable variable = 
    map(\(CElem (Elem _ atts _) _) -> 
            Variable (getValue "org.eventb.core.identifier" atts)
        ) variable

generateSeesContext::[Content i] -> [SeesContext]
generateSeesContext sc =
    map(\(CElem (Elem _ atts _) _) -> 
            SeesContext (getValue "org.eventb.core.target" atts)
        ) sc

textToBoolean::T.Text -> Bool
textToBoolean t 
    | t == (T.pack "1") = True
    | t == (T.pack "0") = False
    | otherwise = False

generateEvent::[Content i] -> [Event]
generateEvent event = 
    foldr (\(CElem (Elem _ atts sl) _) acc -> 
        if (((getValue "org.eventb.core.label" atts) /= (T.pack "INITIALISATION")) )
           then 
                let m = generateBalise sl Map.empty in 
                let 
                    actionBalise = Map.findWithDefault [] "org.eventb.core.action" m
                    guardeBalise = Map.findWithDefault [] "org.eventb.core.guard" m
                    parameterBalise = Map.findWithDefault [] "org.eventb.core.parameter" m
                    refinesBalise = Map.findWithDefault [] "org.eventb.core.refinesEvent" m
                    in (Event (textToBoolean $ getValue "org.eventb.core.convergent" atts) (getValue "org.eventb.core.label" atts) (generateParameter parameterBalise) (generateGarde guardeBalise) (generateAction actionBalise) (generateRefinesEvent refinesBalise)):acc
            else
                acc
        ) [] event

generateInitialisation:: [Content i] -> [Event]
generateInitialisation event = 
    foldr (\(CElem (Elem _ atts sl) _) acc -> 
        if (((getValue "org.eventb.core.label" atts) == (T.pack "INITIALISATION")) )
           then 
                let m = generateBalise sl Map.empty in 
                let 
                    actionBalise = Map.findWithDefault [] "org.eventb.core.action" m
                    guardeBalise = Map.findWithDefault [] "org.eventb.core.guard" m
                    parameterBalise = Map.findWithDefault [] "org.eventb.core.parameter" m
                    refinesBalise = Map.findWithDefault [] "org.eventb.core.refinesEvent" m
                    in (Event (textToBoolean $ getValue "org.eventb.core.convergent" atts) (getValue "org.eventb.core.label" atts) (generateParameter parameterBalise) (generateGarde guardeBalise) (generateAction actionBalise) (generateRefinesEvent refinesBalise)):acc

            else
                acc
        ) [] event

generateRefines:: [Content i] -> [Refines]
generateRefines refines =
    map(\(CElem (Elem _ atts _) _) -> 
            Refines (getValue "org.eventb.core.target" atts)
        ) refines



generateMachineRodinAst :: Map.Map String [Content i] -> MachineInfo {- A finir faire que l'initialisation soit récupérer dans les event et mettre les autres event a part-}
generateMachineRodinAst m =
    let seesContextBalise = Map.findWithDefault [] "org.eventb.core.seesContext" m
        invariantBalise    = Map.findWithDefault [] "org.eventb.core.invariant" m
        variantBalise    = Map.findWithDefault [] "org.eventb.core.variant" m
        eventBalise    = Map.findWithDefault [] "org.eventb.core.event" m
        variableBalise    = Map.findWithDefault [] "org.eventb.core.variable" m
        refinesBalise = Map.findWithDefault [] "org.eventb.core.refinesMachine" m
    in MachineInfo (generateInitialisation eventBalise) (generateSeesContext seesContextBalise) (generateVariable variableBalise) (generateInvariant invariantBalise) (generateVariant variantBalise) (generateRefines refinesBalise) (generateEvent eventBalise)