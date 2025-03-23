module Main (main) where
{-# LANGUAGE OverloadedStrings #-} -- permet d'utiliser Text comme un string avec des " "

import RodinAst.Context (ContextFile, Constant, Axiom)
import LeanMachineAst.ContextToLean
import LeanMachineAst.MachineToLean
import RodinAst.Machine (MachineInfo, SeesContext, Variable, Invariant, Variant, Event, Garde, Action, Parameter)
import Text.XML.HaXml.XmlContent
import LeanMachineAst.ContextLeanAst
import LeanMachineAst.ContextLeanProgram

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
    stds2 <- fReadXml "./xmlFile/Context/testContextPredicat.xml"::IO ContextFile
    let std2 = stds2::ContextFile in
        do  TIO.writeFile"./leanFile/Context/context.lean" (parseContextAst (generateContextAst std2))
    fWriteXml "./xmlFile/Context/testContext-out.xml" stds2
    writeFile "./leanFile/Context/test.lean" "(ml_tl=green ∧ a+b+1&lt;d) ∨ &#10;(ml_tl=green ∧ a+b+1=d) ∨&#10;(il_tl=green ∧ b&gt;1) ∨ &#10;(il_tl=green ∧ b=1) ∨&#10;(ml_tl=red ∧ a+b&lt;d ∧ c=0 ∧ il_pass=1) ∨ &#10;(il_tl=red ∧ 0&lt;b ∧ a=0 ∧ ml_pass=1) ∨&#10;0&lt;a ∨&#10;0&lt;c"




