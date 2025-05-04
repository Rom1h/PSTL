module  LeanMachineAst.ContextLeanProgram where 

import Data.Text (Text, pack, append)

import LeanMachineAst.DataExpr
import qualified LeanMachineAst.ContextLeanAst as CLA

parseContextAst :: CLA.ContextAst -> Text
parseContextAst (CLA.ContextAst cName cons axioms) = 
    pack "import LeanMachines.Event.Basic\n" <>
    pack "import LeanMachines.Event.Ordinary\n" <>
    pack "import LeanMachines.Event.Convergent\n" <>
    pack "import LeanMachines.NonDet.Ordinary\n\n" <>
    pack "-- CONTEXT\n" <>
    pack "structure " <> cName <> pack " where\n" <> parseConstant cons <> parseAxiom axioms

parseConstant :: [CLA.ConstantL] -> Text
parseConstant consList = 
    foldr (\(CLA.ConstantL n t) acc -> 
        pack "\t"<>n <> pack " : " <> cTypeToText t <> pack "\n" <> acc
    ) (pack "") consList

parseAxiom :: [CLA.AxiomL] -> Text
parseAxiom axiomList = 
    foldr (\(CLA.AxiomL n e) acc -> 
        pack "\t" <> n <> pack " : " <> exprToText e <> pack "\n" <> acc
    ) (pack "") axiomList
