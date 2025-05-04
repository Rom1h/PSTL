module Main (main) where
 -- permet d'utiliser Text comme un string avec des " "

import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hSetEncoding, utf8)

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
import LeanMachineAst.MachineLeanProgramme
import Text.XML.HaXml.Types (Content(..), Element(..), QName(..), Attribute,AttValue(..))

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List (isSuffixOf)

getBaseName :: FilePath -> String
getBaseName path =
  let nameWithExt = last (splitOn '/' path)
      name = takeWhile (/= '.') nameWithExt
  in name

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn sep (x:xs)
  | x == sep  = [] : rest
  | otherwise = (x : head rest) : tail rest
  where rest = splitOn sep xs

main :: IO ()
main = do
    args <- getArgs
    taille <- pure $ Prelude.length (args::[String])
    if (taille > 2) || (taille < 2) then 
        putStrLn "Veillez mettre en premier le fichier contexte puis le fichier machine"
    else do
        let ctxPath = args !! 0
            ctxName = getBaseName ctxPath
        xmlContent <- readFile ctxPath
        let doc = xmlParse "Erreur dans le parsing du context" xmlContent 
            Document _ _ rootElem _ = doc
            Elem _ _ children = rootElem
            balisesMap = CRA.generateBalise children Map.empty
            contextAst = generateContextRodinAst (T.pack ctxName) balisesMap
        let machinePath =args !! 1 
            machineName = getBaseName machinePath
        xmlMachine <- readFile machinePath
        let doc2 = xmlParse "Erreur dans le parsing de la machine" xmlMachine 
            Document _ _ rootElem2 _ = doc2 
            Elem _ _ children2 = rootElem2
            baliseMap2 = MRA.generateBalise children2 Map.empty
            machineAst = MRA.generateMachineRodinAst baliseMap2
            contextLeanAST = generateContextAst contextAst
            tContext = parseContextAst contextLeanAST
            tMachine = parseMachineAst contextLeanAST (MLA.generateMachineAst (T.pack machineName) machineAst)
            tProgram = generateProgramm tContext tMachine

        
        withFile "./leanFile/Context/context.lean" WriteMode $ \h1 -> do
            hSetEncoding h1 utf8
            TIO.hPutStr h1 tContext

        withFile "./leanFile/Machine/machine.lean" WriteMode $ \h2 -> do
            hSetEncoding h2 utf8
            TIO.hPutStr h2 tMachine

        withFile "./leanFile/Program/programme.lean" WriteMode $ \h3 -> do
            hSetEncoding h3 utf8
            TIO.hPutStr h3 tProgram

generateProgramm :: T.Text-> T.Text -> T.Text
generateProgramm tContext tMachine = 
    tContext <>
    T.pack "\n\n"<>
    tMachine
