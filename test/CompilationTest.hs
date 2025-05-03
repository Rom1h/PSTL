module CompilationTest where 

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Test.Hspec
import qualified Data.Text as T 
import RodinAst.MachineRodinAst as MRA 
import LeanMachineAst.MachineLeanAst as MLA
import LeanMachineAst.MachineToLean
import RodinAst.ContextRodinAst as CRA 
import LeanMachineAst.ContextLeanAst as CLA
import LeanMachineAst.ContextToLean
import LeanMachineAst.DataExpr
import LeanMachineAst.MachineLeanProgramme
import LeanMachineAst.ContextLeanProgram


import qualified Data.Map as Map

machineRodinASTTest :: Spec 
machineRodinASTTest = 
    describe "Unitary test on Machine Compilation" $ do 
        it "should recognize INITIALISATION with one action" $ do 
            let machinePath = "./xmlFile/test/initialisation.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "initialisation.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit@(MRA.MachineInfo init _ _ _ _ _ _) = MRA.generateMachineRodinAst baliseMap2
            (head init) `shouldBe` (MRA.Event False (T.pack "INITIALISATION") [] [] [MRA.Action (T.pack "count \212\235\246 0")] [])
        it "should generate InitialisationAst" $ do 
            let machinePath = "./xmlFile/test/initialisation.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "initialisation.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit = MRA.generateMachineRodinAst baliseMap2
                (MLA.MachineAst _ initLA _ _ _ _ _) = MLA.generateMachineAst (T.pack "") machInit
            (initLA) `shouldBe` (MLA.InitAst (MLA.EventAst (T.pack "INITIALISATION") [] [(MLA.ActionAst (textToExpr(T.pack "count \212\235\246 0")))]))
        it "should recognize SeesContext with one action" $ do 
            let machinePath = "./xmlFile/test/SeesContext.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "SeesContext.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                MRA.MachineInfo _ sc _ _ _ _ _ = MRA.generateMachineRodinAst baliseMap2
            (head sc) `shouldBe` (MRA.SeesContext (T.pack "BoundedCtx"))
        it "should generate SeesContextAst" $ do 
            let machinePath = "./xmlFile/test/SeesContext.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "SeesContext.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit = MRA.generateMachineRodinAst baliseMap2
                (MLA.MachineAst _ _ scLA _ _ _ _) = MLA.generateMachineAst (T.pack "") machInit
            (scLA) `shouldBe` (MLA.MachineContAst (T.pack "ctx") (T.pack "BoundedCtx") )
        it "should recognize Variables with one action" $ do 
            let machinePath = "./xmlFile/test/Variables.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Variables.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                MRA.MachineInfo _ _ var _ _ _ _ = MRA.generateMachineRodinAst baliseMap2
            (head var) `shouldBe` (MRA.Variable (T.pack "count"))
        it "should generate VariableAst" $ do 
            let machinePath = "./xmlFile/test/Variables.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Variables.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit = MRA.generateMachineRodinAst baliseMap2
                (MLA.MachineAst _ _ _ varLA _ _ _) = MLA.generateMachineAst (T.pack "") machInit
            (head varLA) `shouldBe` (MLA.VariableAst (T.pack "count") (Nat) )

        it "should recognize Invariants with one action" $ do 
            let machinePath = "./xmlFile/test/Invariant.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Invariant.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                MRA.MachineInfo _ _ _ inv _ _ _ = MRA.generateMachineRodinAst baliseMap2
            (head inv) `shouldBe` (MRA.Invariant (T.pack "count_bounded") (T.pack "count \212\235\241 maxCount"))
        it "should generate InvariableAst" $ do 
            let machinePath = "./xmlFile/test/Invariant.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Invariant.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit = MRA.generateMachineRodinAst baliseMap2
                (MLA.MachineAst _ _ _ _ inv _ _) = MLA.generateMachineAst (T.pack "") machInit
            (head inv) `shouldBe` (MLA.InvariantAst (T.pack "count_bounded") (textToExpr (T.pack "count \212\235\241 maxCount")))
        it "should recognize Variants with one action" $ do 
            let machinePath = "./xmlFile/test/Variants.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Variants.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                MRA.MachineInfo _ _ _ _ var _ _ = MRA.generateMachineRodinAst baliseMap2
            (head var) `shouldBe` (MRA.Variant (T.pack "vrn2") (T.pack "count \212\235\246 count \212\234\198 1"))
        it "should generate VariantAst" $ do 
            let machinePath = "./xmlFile/test/Variants.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Variants.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit = MRA.generateMachineRodinAst baliseMap2
                (MLA.MachineAst _ _ _ _ _ var _) = MLA.generateMachineAst (T.pack "") machInit
            (head var) `shouldBe` (MLA.VariantAst (T.pack "vrn2") (textToExpr (T.pack "count \212\235\246 count \212\234\198 1")))
        it "should recognize Events with one action" $ do 
            let machinePath = "./xmlFile/test/Events.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Events.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                MRA.MachineInfo _ _ _ _ _ _ events = MRA.generateMachineRodinAst baliseMap2
            (head events) `shouldBe` (MRA.Event False (T.pack "Discard") [MRA.Parameter (T.pack "k")] [(MRA.Garde (T.pack "grd_type") (T.pack "k \212\234\234 \212\228\242"))] [MRA.Action (T.pack "count \212\235\246 count \212\234\198 k")] [])
        it "should generate EventAst" $ do 
            let machinePath = "./xmlFile/test/Events.xml"
            xmlMachine <- readFile machinePath
            let doc2 = xmlParse "Events.xml" xmlMachine 
                Document _ _ rootElem2 _ = doc2 
                Elem _ _ children2 = rootElem2
                baliseMap2 = MRA.generateBalise children2 Map.empty
                machInit = MRA.generateMachineRodinAst baliseMap2
                (MLA.MachineAst _ _ _ _ _ _ events) = MLA.generateMachineAst (T.pack "") machInit
            (head events) `shouldBe` (MLA.EventAst (T.pack "Discard") [MLA.GardeAst (T.pack "grd_type") (textToExpr (T.pack "k \212\234\234 \212\228\242"))] [MLA.ActionAst (textToExpr (T.pack "count \212\235\246 count \212\234\198 k"))])


contextRodinASTTest :: Spec 
contextRodinASTTest = 
    describe "Unitary test on Context Compilation" $ do 
        it "should recognize Constant with one action" $ do 
            let ctxPath ="./xmlFile/test/Constant.xml" -- mettre head args  à la place testContext.xml
            xmlContent <- readFile ctxPath
            let doc = xmlParse "Constant.xml" xmlContent  -- pareil
                Document _ _ rootElem _ = doc
                Elem _ _ children = rootElem
                balisesMap = CRA.generateBalise children Map.empty
                (CRA.ContextFile _ const _) = generateContextRodinAst (T.pack "") balisesMap
            (head const) `shouldBe` (CRA.Constant (T.pack "maxCount"))
        it "should generate ConstantAst" $ do 
            let ctxPath ="./xmlFile/test/Constant.xml" -- mettre head args  à la place testContext.xml
            xmlContent <- readFile ctxPath
            let doc = xmlParse "Constant.xml" xmlContent  -- pareil
                Document _ _ rootElem _ = doc
                Elem _ _ children = rootElem
                balisesMap = CRA.generateBalise children Map.empty
                context = generateContextRodinAst (T.pack "") balisesMap
                (CLA.ContextAst _ const _ ) = CLA.generateContextAst context
            (head const) `shouldBe` (CLA.ConstantL (T.pack "maxCount") (Nat))
        it "should recognize Axiom with one action" $ do 
            let ctxPath ="./xmlFile/test/Axiom.xml" -- mettre head args  à la place testContext.xml
            xmlContent <- readFile ctxPath
            let doc = xmlParse "Axiom.xml" xmlContent  -- pareil
                Document _ _ rootElem _ = doc
                Elem _ _ children = rootElem
                balisesMap = CRA.generateBalise children Map.empty
                (CRA.ContextFile _ _ ax) = generateContextRodinAst (T.pack "") balisesMap
            (head ax) `shouldBe` (CRA.Axiom (T.pack "prop_maxCount") (T.pack "maxCount &gt; 0"))
        it "should generate AxiomAst" $ do 
            let ctxPath ="./xmlFile/test/Axiom.xml" -- mettre head args  à la place testContext.xml
            xmlContent <- readFile ctxPath
            let doc = xmlParse "Axiom.xml" xmlContent  -- pareil
                Document _ _ rootElem _ = doc
                Elem _ _ children = rootElem
                balisesMap = CRA.generateBalise children Map.empty
                context = generateContextRodinAst (T.pack "") balisesMap
                (CLA.ContextAst _ _ ax) = CLA.generateContextAst context
            (head ax) `shouldBe` (CLA.AxiomL (T.pack "prop_maxCount") (textToExpr (T.pack "maxCount &gt; 0")))