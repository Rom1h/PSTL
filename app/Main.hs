module Main (main) where
 -- permet d'utiliser Text comme un string avec des " "

import RodinAst.ContextRodinAst (ContextFile, Constant, Axiom)
import LeanMachineAst.ContextToLean
import LeanMachineAst.MachineToLean as MTL
import RodinAst.MachineRodinAst (MachineInfo, SeesContext, Variable, Invariant, Variant, Event, Garde, Action, Parameter)
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import RodinAst.ContextRodinAst as CRA
import RodinAst.MachineRodinAst as MRA
import qualified Data.Map as Map


import LeanMachineAst.ContextLeanAst
import LeanMachineAst.MachineLeanAst as MLA
import LeanMachineAst.ContextLeanProgram
import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Attribute,AttValue(..))

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--main :: IO ()
--main = do
  --  stds <- fReadXml "example.xml"::IO Students
  --  let std = stds::Students in 
  --      do print ( toStrings std )
{-
main :: IO ()
main = do
    xmlContent <-  readFile "./xmlFile/Context/testContext.xml"
    let doc = (xmlParse "test.txt" xmlContent))


    stds <- fReadXml "./xmlFile/Machine/fichierNormal.xml"::IO MachineInfo
    let std = stds::MachineInfo in
        do writeFile "./leanFile/Machine/machine.lean" (show (generateMachineAst std))

    fWriteXml "./xmlFile/Machine/testMachine-out.xml" stds
    stds2 <- fReadXml "./xmlFile/Context/testContextPredicat.xml"::IO ContextFile
    let std2 = stds2::ContextFile in
        do  TIO.writeFile"./leanFile/Context/context.lean" (parseContextAst (generateContextAst std2))
    fWriteXml "./xmlFile/Context/testContext-out.xml" stds2
    writeFile "./leanFile/Context/test.lean" "(ml_tl=green ∧ a+b+1&lt;d) ∨ &#10;(ml_tl=green ∧ a+b+1=d) ∨&#10;(il_tl=green ∧ b&gt;1) ∨ &#10;(il_tl=green ∧ b=1) ∨&#10;(ml_tl=red ∧ a+b&lt;d ∧ c=0 ∧ il_pass=1) ∨ &#10;(il_tl=red ∧ 0&lt;b ∧ a=0 ∧ ml_pass=1) ∨&#10;0&lt;a ∨&#10;0&lt;c"


-}


main :: IO ()
main = do
    xmlContent <- readFile "./xmlFile/Context/testContext.xml"
    let doc = xmlParse "testContext.xml" xmlContent
        Document _ _ rootElem _ = doc
        Elem _ _ children = rootElem
        balisesMap = CRA.generateBalise children Map.empty
        contextAst = generateContextRodinAst balisesMap
    xmlMachine <- readFile "./xmlFile/Machine/fichierNormal.xml"
    let doc2 = xmlParse "fichierNormal.xml" xmlMachine
        Document _ _ rootElem2 _ = doc2 
        Elem _ _ children2 = rootElem2
        baliseMap2 = MRA.generateBalise children2 Map.empty
        machineAst = MRA.generateMachineRodinAst baliseMap2

    TIO.writeFile"./leanFile/Context/context.lean" (parseContextAst (generateContextAst contextAst))
    TIO.writeFile"./leanFile/Machine/machine.lean" (T.pack (show (MLA.generateMachineAst machineAst)))

    
