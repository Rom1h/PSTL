module Main (main) where

import Context (ContextFile, Constant, Axiom)
import Machine (MachineInfo, SeesContext, Variable, Invariant, Variant, Event, Garde, Action, Parameter)
import Text.XML.HaXml.XmlContent

--main :: IO ()
--main = do
  --  stds <- fReadXml "example.xml"::IO Students
  --  let std = stds::Students in 
  --      do print ( toStrings std )
    
main :: IO ()
main = do
    stds <- fReadXml "./xmlFile/Machine/testMachine.xml"::IO MachineInfo
    print stds
    fWriteXml "./xmlFile/Machine/testMachine-out.xml" stds
    stds2 <- fReadXml "./xmlFile/Context/testContext.xml"::IO ContextFile
    print stds2
    fWriteXml "./xmlFile/Context/testContext-out.xml" stds2
