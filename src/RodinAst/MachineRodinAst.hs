{-# LANGUAGE OverloadedStrings #-}

module RodinAst.ContextRodinAst where

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
    events::[Event]
    } 

newtype SeesContext = SeesContext
    {target::Text}

data Variable = Variable {name::Text} 
data Invariant = Invariant {labelInv::Text,predicateInv::Text} deriving(Show)
data Variant = Variant {labelVar :: Text , expr :: Text} 
data Event = Event {convergent :: Bool,labelEvent :: Text,parameter :: [Parameter],gards ::[Garde],action:: [Action]} 
data Garde = Garde {labelGarde :: Text ,predicateGarde :: Text} deriving (Show)

newtype Parameter = Parameter 
    {identificateur :: Text}
    deriving Show

data Action = Action 
    {assign :: Text}
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


generateAction :: [Content i] -> [Action]
generateAction action = 
    map(\(CElem (Elem _ atts _) _) -> 
            Action (getValue "org.eventb.core.assignment" atts) {- Pas besoin de stocker le label -}
        ) action

generateParameter::[Content i] -> [Parameter]
generateParameter param = 
    map(\(CElem (Elem _ atts _) _) -> 
            Parameter (getValue "org.eventb.core.identifier")
        ) param

generateGarde::[Content i] -> [Garde]
generateGarde gard =
    map(\(CElem (Elem _ atts _) _) -> 
            Garde (getValue "org.eventb.core.label") (getValue "org.event.core.predicate")
        ) gard

generateVariant::[Content i] -> [Variant]
generateVariant variant = 
    map(\(CElem (Elem _ atts _) _) -> 
            Variant (getValue "org.eventb.core.label") (getValue "org.eventb.core.expression")
        ) variant

generateInvariant::[Content i] -> [Invariant]
generateInvariant invariant = 
    map(\(CElem (Elem _ atts _) _) -> 
            Invariant (getValue "org.eventb.core.label") (getValue "org.eventb.core.predicate")
        ) invariant

generateVariable::[Content i] -> [Variable]
generateVariable variable = 
    map(\(CElem (Elem _ atts _) _) -> 
            Variable (getValue "org.eventb.core.identifier")
        ) variable

generateSeesContext::[Content i] -> [SeesContext]
generateSeesContext sc =
    map(\(CElem (Elem _ atts _) _) -> 
            SeesContext (getValue "org.eventb.core.target")
        ) sc

textToBoolean::Text -> Bool
textToBoolean t 
    | t == (toText "1") = True
    | t == (toText "0") = False
    | otherwise = False

generateEvent::[Content i] -> [Event]
generateEvent event = 
    map(\(CElem (Elem (_ atts sl) _)) -> 
            let 
                actionBalise = Map.findWithDefault [] "org.eventb.core.action" sl
                guardeBalise = Map.findWithDefault [] "org.eventb.core.guard" sl
                parameterBalise = Map.findWithDefault [] "org.eventb.core.parameter" sl
            in Event (textToBoolean $ getValue "org.eventb.core.convergent") (getValue "org.eventb.core.label") (generateParameter parameterBalise) (generateGarde guardeBalise) (generateAction actionBalise)
        ) event

generateMachineRodinAst :: Map.Map String [Content i] -> ContextFile {- A finir faire que l'initialisation soit récupérer dans les event et mettre les autres event a part-}
generateMachineRodinAst m =
    let seesContextBalise = Map.findWithDefault [] "org.eventb.core.seesContext" m
        invariantBalise    = Map.findWithDefault [] "org.eventb.core.invariant" m
        variantBalise    = Map.findWithDefault [] "org.eventb.core.variant" m
        eventBalise    = Map.findWithDefault [] "org.eventb.core.event" m
        variableBalise    = Map.findWithDefault [] "org.eventb.core.variable" m
    in MachineInfo (generate constantsBalise) (generateAxiom axiomsBalise)