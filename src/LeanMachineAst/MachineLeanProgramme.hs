module LeanMachineAst.MachineLeanProgramme where 

import Data.Text (Text, pack, append)

import LeanMachineAst.DataExpr
import qualified LeanMachineAst.MachineLeanAst as MLA

parseMachineAst :: MLA.MachineAst -> Text
parseMachineAst (MLA.MachineAst mName mInit mContext variable invariants variants events) =
    pack "structure" <> mName <>
    pack "/-SEES-/ (ctx:" <> toTextContType mContext <>
    pack ") where \n\t" <> 
    pack ""

toTextContType :: MLA.MachineContAst -> Text
toTextContType (MLA.MachineContAst cName cType) = cType



