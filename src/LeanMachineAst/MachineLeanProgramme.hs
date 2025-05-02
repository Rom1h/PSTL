{-# LANGUAGE OverloadedStrings #-}

module LeanMachineAst.MachineLeanProgramme where 

import Data.Text (Text, pack, append,intercalate,all)
import Data.Char (isDigit)
import qualified Data.Text as T

import LeanMachineAst.DataExpr
import qualified LeanMachineAst.MachineLeanAst as MLA
import qualified LeanMachineAst.ContextLeanAst as CLA


parseMachineAst :: CLA.ContextAst -> MLA.MachineAst -> Text
parseMachineAst ctx@(CLA.ContextAst ctxName consL axs) m@(MLA.MachineAst mName mInit mContext variable invariants variants events) =
    
    let ctxConsName = map (\(CLA.ConstantL consName _ )-> consName) consL in
    "-- MACHINE \n"<>
    "structure:  " <> mName <>
    pack "/-SEES-/ (ctx:" <> (toTextContType mContext) <>
    pack ") where" <> 
    (parseVariables variable)<>
    pack "\n\n"<>
    pack "namespace "<> mName<>"\n\n"<>
    (parseInvariant ctxConsName mName invariants) <>
    pack "\n"<>
    (parseVariant ctxConsName mName variants)<>
    pack "\n"<>
    parseDefault mName variable <>
    pack "\n\n"<>
    (generateInstanceMachine m)<>
    pack "\n\n"<>

    parseMachineInit mInit mName <>
    pack "\n\n"<>

    parseEvents ctxConsName mName events



generateInstanceMachine :: MLA.MachineAst -> Text
generateInstanceMachine (MLA.MachineAst mName _ mContext _ invariants variants _) =
    pack "instance: Machine " <> mName <> " (" <> mName <> " ctx) where\n"
    <> pack "  context := ctx\n"
    <> (if not (null invariants)
        then pack "  invariant m := " <> intercalate " ∧ " (map (\(MLA.InvariantAst name _) -> name) invariants) <> pack "\n"
        else mempty)
    <> (if not (null variants)
        then pack "  variant m := " <> intercalate " ∧ " (map (\(MLA.VariantAst name _) -> name) variants) <> pack "\n"
        else mempty)
    <> pack "  default := Default"

toTextContType :: MLA.MachineContAst -> Text
toTextContType (MLA.MachineContAst cName cType) = cType

parseMachineInit:: MLA.InitialisationAst -> Text-> Text
parseMachineInit (MLA.InitAst (MLA.EventAst name gardes actions)) mName =
    let actText = foldr (\(MLA.ActionAst act) acc ->     
            "\n\t\t\t"<>(exprToText act)<>acc) "" actions
            in
    "def Initialisation : InitEvent ("<>mName<>" ctx) Unit Unit :="<>
    "\n\tnewInitEvent'' {"<>
       "\n\t\t init _ :=" <>"{"<>actText<>" }"<>
        "\n\t\tsafety _ := by sorry "<>
  "\n\t}"

parseMachineContext :: MLA.MachineContAst -> Text
parseMachineContext cst = undefined

parseVariables ::[MLA.VariableAst] -> Text
parseVariables vars =
    foldr (\(MLA.VariableAst name t) acc-> acc<>"\n\t"<>name <>":"<>cTypeToText t) "" vars

parseDefault :: Text ->[MLA.VariableAst] -> Text
parseDefault name vars = 
    "@[simp]\n"<>
    "def Default : "<>name<> "ctx :="<>
    "\n\t{"<>foldr (\(MLA.VariableAst n _) acc ->     
            "\n\t"<>n<>" := default"<>acc) "" vars<>
    "}"

parseVariant ::[Text]->Text-> [MLA.VariantAst] -> Text
parseVariant ctxConsName mName variants = 
        foldr (\(MLA.VariantAst name expr) acc->
                    let exprContext = changeExprByContext expr ctxConsName in
                    acc<>"\n@[simp]\ndef "<>name <>" (m : "<> mName <>" ctx) : Prop :=\n\t"<>exprToText exprContext) "" variants


parseInvariant ::[Text]-> Text-> [MLA.InvariantAst] -> Text
parseInvariant ctxConsName mName invariants = 
        foldr (\(MLA.InvariantAst name expr) acc->     
            let exprContext = changeExprByContext expr ctxConsName in
            acc<>"\n@[simp]\ndef "<>name <>" (m : " <>  mName <> " ctx) : Prop :=\n\t"<>exprToText exprContext) "" invariants


parseEvents :: [Text]->Text->[MLA.EventAst] -> Text
parseEvents ctxConsName mName events =
    foldr (\e acc -> acc<>"\n"<> parseEvent ctxConsName mName e) "" events

parseGarde :: [Text]->Text->Text-> MLA.GardeAst -> Text
parseGarde ctxConsName name mName (MLA.GardeAst gName prop) =
    let exprContext = changeExprByContext prop ctxConsName in
    "@[simp]\n"<>
    "def "<>name<>"."<>gName <> " (m : "<>mName<>" ctx) : Prop :="<>
    "\n\t"<>exprToText exprContext


parseGardes :: [Text]->Text->Text->[MLA.GardeAst] -> Text
parseGardes ctxConsName name mName listGardes =
    foldr (\garde acc-> acc <>"\n\n"<> (parseGarde ctxConsName name mName garde)) "" listGardes

parseActions :: [Text]->Text->Text-> [MLA.ActionAst] ->Text
parseActions ctxConsName name mName actionsL = 
    "@[simp]\n"<>
    "def "<> name<>".action (m : "<>mName<>" ctx) : "<>mName<>" ctx :=\n"<>
    "\t{"<>foldr (\(MLA.ActionAst act) acc ->     
            let exprContext = changeExprByContext act ctxConsName in
            "\n\t"<>(exprToText exprContext)<>acc) "" actionsL<>
    "}"


parseEvent ::[Text]->Text-> MLA.EventAst -> Text
parseEvent ctxConsName mName (MLA.EventAst name gardes actions) = 
    parseGardes ctxConsName name mName gardes <> "\n\n" <>
    parseActions ctxConsName name mName actions <> "\n\n" <>
    "def "<>name <>": OrdinaryEvent ("<>mName<>" ctx) Unit Unit :=\n\t"<>
    "newEvent'' {" <> 
    "\n\t\tguard m :="<>(intercalate " ∧ " (map (\(MLA.GardeAst nG _) ->  name<>"."<>nG<>" m") gardes))<>
    "\n\t\taction m _ := "<>name<>"."<>"action m"<>
    "\n\t\tsafety m := by sorry\n"<>
    "\t}"


isNumber :: Text -> Bool
isNumber x = T.all isDigit x

changeExprByContext :: Expr -> [Text] -> Expr
changeExprByContext expr ctxNames = case expr of
  Eq e1 e2     -> Eq     (go e1) (go e2)
  NotEq e1 e2  -> NotEq  (go e1) (go e2)
  Inf e1 e2    -> Inf    (go e1) (go e2)
  InfEq e1 e2  -> InfEq  (go e1) (go e2)
  Sup e1 e2    -> Sup    (go e1) (go e2)
  SupEq e1 e2  -> SupEq  (go e1) (go e2)
  Impliq e1 e2 -> Impliq (go e1) (go e2)
  Or e1 e2     -> Or     (go e1) (go e2)
  And e1 e2    -> And    (go e1) (go e2)
  Add e1 e2    -> Add    (go e1) (go e2)
  Sub e1 e2    -> Sub    (go e1) (go e2)
  Mul e1 e2    -> Mul    (go e1) (go e2)
  In e ty      -> In     (go e) ty
  Aff e1 e2    -> Aff    (go e1) (go e2)
  Cons x
    | isNumber x -> Cons x  
    | x `elem` ctxNames -> Cons ("ctx." <> x)
    | otherwise         -> Cons ("m." <> x)
  where
    go = (`changeExprByContext` ctxNames)