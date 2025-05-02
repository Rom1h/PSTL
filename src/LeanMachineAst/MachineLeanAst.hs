module LeanMachineAst.MachineLeanAst where
import LeanMachineAst.DataExpr
import RodinAst.MachineRodinAst (MachineInfo(..), SeesContext(..), Variable(..), Invariant(..), Variant(..), Event(..), Garde(..), Parameter(..), Action(..))
import qualified Data.Text as T
import Data.Text (Text)


data MachineAst = MachineAst{
    machineName::Text, 
    machineInit::InitialisationAst,
    machineContext::MachineContAst,
    variables::[VariableAst], 
    invariants::[InvariantAst],
    variant:: [VariantAst],
    events::[EventAst]
    } 

data MachineContAst = MachineContAst{
    contName::Text,
    contType::Text
}deriving (Show)

data VariantAst = VariantAst{
    variantName::Text,
    variantExpr::Expr
}deriving (Show)

data VariableAst = VariableAst{
    variableName::Text,
    variableType::CType
}deriving (Show)

data InvariantAst = InvariantAst{
    invariantName::Text,
    invariantProp::Expr
}deriving (Show)

data InitialisationAst = InitAst{
    initialisationEvent::EventAst
}deriving (Show)

data EventAst = EventAst{
    eventName :: Text,
    eventGard :: [GardeAst],
    eventAction :: [ActionAst]
}deriving (Show)

data GardeAst = GardeAst{
    guardeName::Text,
    guardeProp:: Expr
}deriving (Show)

data ActionAst = ActionAst{
    actionExpr:: Expr
}deriving (Show)

generateMachineAst:: Text->MachineInfo -> MachineAst
generateMachineAst mName (MachineInfo is scs vars invs varis _ events) = 
    let (res1, res2) = getTypeInv invs in 
        MachineAst  mName (generateInitialisationAst is) (generateMachineConstAst scs) (generateVariableAst vars res1) (generateInvariantAst res2) (generateVariantAst varis) (generateEventAst events)

generateInitialisationAst::[Event] -> InitialisationAst
generateInitialisationAst [Event c l p g a _ ] = InitAst (EventAst l (generateGardeAst g) (generateActionAst a))

generateVariantAst:: [Variant] -> [VariantAst]
generateVariantAst [] = []
generateVariantAst ((Variant l expr):vs) = VariantAst l (textToExpr expr):(generateVariantAst vs)

generateMachineConstAst::[SeesContext] -> MachineContAst
generateMachineConstAst [(SeesContext t)] = MachineContAst (T.pack "ctx") (t)

generateVariableAst::[Variable] -> [Invariant] -> [VariableAst] 
generateVariableAst vars invs = foldr (\(Variable n) acc -> ((VariableAst n (getType n invs)):acc)) [] vars

generateInvariantAst::[Invariant] -> [InvariantAst]
generateInvariantAst [] = []
generateInvariantAst ((Invariant l p):invs) = (InvariantAst l (textToExpr p)):(generateInvariantAst invs)

generateEventAst::[Event] -> [EventAst]
generateEventAst [] = []
generateEventAst ((Event _ l _ g a _):evs) = ((EventAst l (generateGardeAst g) (generateActionAst a)):(generateEventAst evs))

generateGardeAst::[Garde] -> [GardeAst]
generateGardeAst [] = []
generateGardeAst ((Garde l p):gs) = ((GardeAst l (textToExpr p)):(generateGardeAst gs))


generateActionAst::[Action] -> [ActionAst]
generateActionAst [] = []
generateActionAst ((Action ass):gs) = ((ActionAst (textToExpr ass)):(generateActionAst gs))

getType::Text -> [Invariant] -> CType
getType _ [] = Nat
getType varName ((Invariant l p):invs) 
    | (varName /= T.empty) && (T.isInfixOf varName l) = textToType (Prelude.last (T.splitOn (T.pack "∈") p))
    | otherwise = (getType varName invs)


isInvType::Text -> Bool
isInvType t
    | (T.null t) = False
    | (T.head t) == '_' = T.tail t == (T.pack "type")
    | otherwise = isInvType (T.tail t) 

getTypeInv:: [Invariant] -> ([Invariant], [Invariant])
getTypeInv = foldr (\(Invariant l p) (acc1, acc2) -> if isInvType l then ((Invariant l p):acc1, acc2) else (acc1, (Invariant l p):acc2)) ([], [])