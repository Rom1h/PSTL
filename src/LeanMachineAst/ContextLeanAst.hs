module LeanMachineAst.ContextLeanAst where
import LeanMachineAst.DataExpr
import RodinAst.Context (ContextFile(..), Constant(..), Axiom(..))
import Data.Text (Text, pack)


-- Changer nom data ConstantL / AxiomL 
data ContextAst = ContextAst {contextName ::Text ,constants :: [ConstantL],axioms::[AxiomL]} deriving (Show)
data ConstantL = ConstantL {constantName :: Text , constantType :: CType} deriving (Show)
data AxiomL = AxiomL {axiomName :: Text, axiom :: Expr} deriving (Show)


--Name à modifier (trouver comment le récuperer)
generateContextAst :: ContextFile -> ContextAst
generateContextAst (ContextFile cons ax) = 
    ContextAst (pack "Name") (generateConstantsAst cons ax)  (generateAxiomAst ax)

--getConstType récupere le type (type_cons) de la constante dans la liste des Axiom et convertie le type String en CType.
generateConstantsAst :: [Constant]->[Axiom]->[ConstantL]
generateConstantsAst consList axiomList = map (\(Constant id) -> ConstantL  (pack id)  (getConsType (pack id) axiomList) ) consList

generateAxiomAst :: [Axiom] -> [AxiomL]
generateAxiomAst axiomList = map (\(Axiom l p) -> AxiomL (pack l) (textToExpr (pack p))) axiomList

getConsType :: Text->[Axiom]->CType
getConsType t axiomList = undefined





