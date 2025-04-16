import LeanMachines.Event.Basic
import LeanMachines.Event.Ordinary
import LeanMachines.Event.Convergent
import LeanMachines.NonDet.Ordinary

-- CONTEXT
structure BoundedCtx where
structure Name where
maxCount : Nat
type_maxCount : maxCount 