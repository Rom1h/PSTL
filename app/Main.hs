module Main (main) where
{-# LANGUAGE OverloadedStrings #-} -- permet d'utiliser Text comme un string avec des " "

import RodinAst.Context (ContextFile, Constant, Axiom)
import LeanMachineAst.ContextToLean
import LeanMachineAst.MachineToLean
import RodinAst.Machine (MachineInfo, SeesContext, Variable, Invariant, Variant, Event, Garde, Action, Parameter)
import Text.XML.HaXml.XmlContent

--main :: IO ()
--main = do
  --  stds <- fReadXml "example.xml"::IO Students
  --  let std = stds::Students in 
  --      do print ( toStrings std )

main :: IO ()
main = do
    stds <- fReadXml "./xmlFile/Machine/fichierNormal.xml"::IO MachineInfo
    let std = stds::MachineInfo in
        do writeFile "./leanFile/Context/machine.lean" (show std)

    fWriteXml "./xmlFile/Machine/testMachine-out.xml" stds
    stds2 <- fReadXml "./xmlFile/Context/testContext.xml"::IO ContextFile
    let std2 = stds2::ContextFile in
        do  writeFile "./leanFile/Context/context.lean" (show std2)
    fWriteXml "./xmlFile/Context/testContext-out.xml" stds2

