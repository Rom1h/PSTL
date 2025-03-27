module LeanMachineAst.MachineLeanAst where
import LeanMachineAst.DataExpr
import RodinAst.Machine (MachineInfo(..), SeesContext(..), Variable(..), Invariant(..), Variant(..), Event(..), Garde(..), Parameter(..), Action(..))
import qualified Data.Text as T
import Data.Text (Text)


data MachineAst = MachineAst{
    machineName::Text, 
    machineContext::MachineContAst,
    variabless::[VariableAst], 
    invariants::[InvariantAst],
    machineInit::InitialisationAst
    events::[EventAst],
    }

data MachineContAst = MachineContAst{
    contName::Text,
    contType::Text
}

data VariableAst = VariableAst{
    variableName::Text,
    variableType::CType
}

data InvariantAst = InvariantAst{
    invariantName::Text,
    invariantProp::Expr
}

data InitialisationAst = InitAst{
    initialisationName::Text,
    initialisationInit::Expr
}

data EventAst = EventAst{
    eventGuard = [GuardeAst],
    eventAction = [ActionAst]
}

data GuardeAst = GuardeAst{
    guardeName::Text,
    guardeProp:: Expr
}

data ActionAst = ActionAst{
    actionExpr:: Expr,
}

generateMachineAst:: MachineInfo -> MachineAst
generateMachineAst (MachineInfo is scs vars invs varis events) = 
    let (invTypes, invs) = getTypeInv invs in 
        MachineAst (generateInitialisationAst is) (generateMachineConstAst scs) (generateVariableAst vars invTypes) (generateInvariantAst invs) (generateVariantAst varis) (generateEventAst events)

generateInitialisationAst::[Event] -> InitialisationAst
generateInvariantAst [(Event c l p g (Action ass))] = InitialisationAst (T.pack "Initialisation") ass

generateMachineConstAst::[SeesContext] -> MachineContAst
generateMachineConstAst [(SeesContext t)] = MachineContAst (T.pack "ctx") (t)

generateVariableAst::[Variable] -> [Invariant] -> [VariableAst] 
generateVariableAst vars invs = foldr (\(Variable n) acc -> (VariableAst n (getType n invs))) [] vars

generateInvariantAst::[Invariant] -> [InvariantAst]
generateInvariantAst ((Invariant l p):invs) = (InvariantAst l (textToExpr p)):(generateInvariantAst invs)

generateEventAst::[Event] -> [EventAst]
generateEventAst ((Event _ _ _ g a):evs) = ((EventAst (generateGuardeAst g) (generateActionAst a)):(generateEventAst evs))

generateGuardeAst::[Guarde] -> [GuardeAst]
generateGuardeAst ((Guarde l p):gs) = ((GuardeAst l (textToExpr p)):(generateGuardeAst gs))


generateActionAst::[Action] -> [ActionAst]
generateActionAst ((Action ass):gs) = ((ActionAst (textToExpr ass)):(generateActionAst gs))

getType::Text -> [Invariant] -> CType
getType varName ((Invariant l p):invs) 
    | (varName != T.empty) && (T.isInfixOf varName l) = textToExpr (Prelude.last (T.splitOn (T.pack "∈" p)))
    | otherwise = (getType varName invs)

isInvType::Text -> Bool
isInvType T.empty = False
isInvType t
    | (T.head t) == '_' = T.tail t == "type"
    | otherwise = isInvType (T.tail t) 

getTypeInv:: [Invariant] -> [Invariant]
getTypeInv = foldr (\(Invariant l p) (acc1, acc2) -> if isInvType l then (Invariant l p) <> acc1 else (Invariant l p) <> acc2) ([], []) 
