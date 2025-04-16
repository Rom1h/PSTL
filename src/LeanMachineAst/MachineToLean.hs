module LeanMachineAst.MachineToLean where

import RodinAst.MachineRodinAst (MachineInfo(..), SeesContext(..), Variable(..), Invariant(..), Variant(..), Event(..), Garde(..), Action(..), Parameter(..))

import Data.Text

showTypage::Variable -> [Invariant] -> ([Invariant],String)
showTypage (Variable name) ((Invariant l p):invs) 
    | (l == (name<>(pack "_type"))) && (p == (name <> (pack" \212\234\234 \212\228\242"))) = (invs, (show name)<>" : Nat")
    | otherwise = showTypage (Variable name) invs

showDefault::[Variable] -> String
showDefault [(Variable name)] = show name <>" := default"
showDefault ((Variable name):vs) = show name <>" := default\n\t" <> showDefault vs

zipTypage:: [Invariant] -> String -> ([Invariant], String)
zipTypage inv s = (inv, s)

showTypageTotal::[Invariant] -> [Variable] -> ([Invariant],[String]) -> ([Invariant], [String])
showTypageTotal inv [] (invb, sb) = ([], [])
showTypageTotal inv (v:vs) (invb, sb) = 
    let (invr, sr) = showTypage v inv in 
        (invr, [sr]<>sb)

--instance Show MachineInfo where 
  --  show (MachineInfo init sC [variable] invs variants e)= "structure Bounded /-SEES-/ (ctx:"<>show sC <>") where" <> (showTypage variable inv) <> show sC <> show variable <> show init <> show e 

instance Show MachineInfo where 
    show (MachineInfo inits [(SeesContext t)] variable invs _ _ e)=
        let init = Prelude.head inits in
        let (invs2, res) = (showTypageTotal invs variable ([],[])) in
            "structure Bounded /-SEES-/ (ctx:"<> show t <>") where\n"<>
            "\t" <> showListeString res <> "\n"<>
            "\nnamespace Bounded\n"<>
            "\n@[simp]\n" <> 
            showListe invs2 <> "\n"<>
            "@[simp]\n"<>
            "def Default : Bounded ctx := \n"<>
            "\t{" <> showDefault variable <>" }"<>"\n"<>
            "\ninstance: Machine " <> show t <>" (Bounded ctx) where\n"<>
            "\tcontext := ctx\n"<>
            "\tinvariant m := " <> showListePredicat (showInvNamePredicat invs2)<>"\n"<>
            "\tdefault := Default" <> "\n"<>
            "\n def " <> showEventLabel init <> ": InitEvent (Bounded ctx) Unit Unit := \n"<>
            "\tnewInitEvent'' {\n"<>
            "\t\t init _ := { "<> showEventAction init <> " }\n"<>
            "\t\t safety _ := by sorry \n"<>
            "\t}\n"<>
            "\n" <> showListe e

instance Show SeesContext where 
    show (SeesContext t) = show t

showGarde :: ([Garde],String) -> String
showGarde ([(Garde l p)], s) = "def " <>s<>".guard_"<> show l <>" (m : Bounded ctx) : Prop := \n\t m."<> showPredicate (show p) <> "\n\n"
showGarde (((Garde l p):gs), s) =  "def " <>s<>".guard_"<> show l <>" (m : Bounded ctx) : Prop := \n\t m."<> showPredicate (show p) <> "\n\n"<> showGarde (gs, s)

showGardeEvent :: [Garde] -> String -> String -- Trouver un meilleur nom si possible
showGardeEvent [(Garde l _)] s = s<>".guard_"<>show l<>" m"
showGardeEvent ((Garde l _):gs) s = s<>".guard_"<>show l<>" m^"<> showGardeEvent gs s

showActionEvent :: [Action] -> String -> String -- Trouver un meilleur nom si possible
showActionEvent [(Action l)] s = s<>".action_"<>show l<>" m"
showActionEvent ((Action l):as) s = s<>".action_"<>show l<>" m^"<> showActionEvent as s


instance Show Event where 
    show (Event c l p g a _) = showGarde (g, show l) <> showAction (a,show l) <> "def "<>show l<>" : OrdinaryEvent (Bounded ctx) Unit Unit := \n\t newEvent'' {\n\t\t"<> "guard m :="<> showGardeEvent g (show l)<>"\n\t\taction m _ := "<> showActionEvent a (show l) <> "\n\t\tsafety m := by sorry\n}"

showAction :: ([Action], String) -> String
showAction ([(Action a)], s) = "def "<>s<>".action (m : Bounded ctx) : Bounded ctx := \n\t { "<>show a <>" }\n\n"
showAction (((Action a):as), s) = "def "<>s<>".action (m : Bounded ctx) : Bounded ctx := \n\t { "<>show a <>" }\n\n" <> showAction (as, s)

showEventLabel:: Event -> String 
showEventLabel (Event _ l _ _ _ _) = show l

showEventAction:: Event -> String 
showEventAction (Event _ _ _ _ ((Action a):as) _) = show a 

showPredicate:: String -> String
showPredicate [s] = [s]
showPredicate (s:ss)
    | (s == '<') = " < ctx." <> showPredicate (ss)
    | (s == '\212') = " \212\235\241 ctx." <> showPredicate (ss)
    | otherwise = [s] <> (showPredicate ss)

showListe ::(Show a) => [a] -> String
showListe [i] = show i
showListe (i:is) = show i <>"\n\n"<> showListe is

showListeString ::[String] -> String
showListeString [i] = i
showListeString (i:is) = i <> showListeString is

showListePredicat :: [String] -> String
showListePredicat [s] = "("<>s<>")"
showListePredicat (s:ss) = ("("<>s<>") ^ ") <> showListePredicat ss

showInvNamePredicat :: [Invariant] -> [String]
showInvNamePredicat [(Invariant l p)] = [show p]
showInvNamePredicat ((Invariant l p):invs) = [("inv_"<>show p)] <> showInvNamePredicat invs

instance Show Invariant where 
    show (Invariant l p) = "def inv_"<> show l <> "(m : BoundedCtx) : Prop :=\n\tm."<> showPredicate (show p) <>"\n"