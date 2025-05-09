module LeanMachineAst.ContextLeanAst where
import LeanMachineAst.DataExpr
import RodinAst.ContextRodinAst (ContextFile(..), Constant(..), Axiom(..))
import qualified Data.Text as T
import Data.Text (Text)


-- Changer nom data ConstantL / AxiomL 
data ContextAst = ContextAst {contextName ::Text ,constants :: [ConstantL],axioms::[AxiomL]} deriving (Show, Eq)
data ConstantL = ConstantL {constantName :: Text , constantType :: CType} deriving (Show, Eq)
data AxiomL = AxiomL {axiomName :: Text, axiom :: Expr} deriving (Show, Eq)


--Name à modifier (trouver comment le récuperer)
generateContextAst :: ContextFile -> ContextAst
generateContextAst (ContextFile ctxName cons ax) = 
    ContextAst ctxName (generateConstantsAst cons ax)  (generateAxiomAst ax)

--getConstType récupere le type (type_cons) de la constante dans la liste des Axiom et convertie le type String en CType.
generateConstantsAst :: [Constant]->[Axiom]->[ConstantL]
generateConstantsAst consList axiomList = map (\(Constant id) -> ConstantL  id  (getConsType id axiomList)) consList

generateAxiomAst :: [Axiom] -> [AxiomL]
generateAxiomAst axiomList = map (\(Axiom l p) -> AxiomL l (textToExpr p)) (removeTypeAxiom axiomList)

removeTypeAxiom :: [Axiom] -> [Axiom]
removeTypeAxiom [] = []
removeTypeAxiom ((Axiom l p) : xs)= 
  if getFlag l == T.pack "type" then removeTypeAxiom xs
  else (Axiom l p):removeTypeAxiom xs
  
getConsType :: Text -> [Axiom] -> CType
getConsType t [] = Nat
getConsType t (Axiom l p : xs) =
    if t == getName l && getFlag l == T.pack "type"
    then textToType p
    else getConsType t xs

getName :: Text -> Text
getName txt =
  case T.uncons txt of
    Nothing -> T.empty
    Just (x, xs)
      | x == '_'  -> xs
      | otherwise -> getName xs

getFlag :: Text -> Text
getFlag txt =
  case T.uncons txt of
    Nothing -> T.empty
    Just (x, xs)
      | x == '_'  -> T.empty
      | otherwise -> T.cons x (getFlag xs)




