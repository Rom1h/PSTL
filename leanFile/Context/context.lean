import LeanMachines.Event.Basic
import LeanMachines.Event.Ordinary
import LeanMachines.Event.Convergent
import LeanMachines.NonDet.Ordinary

-- CONTEXT
structure BoundedCtx where
-- CONSTANTS
maxCount: Nat
type_maxCount : maxCount ∈ ℕ

-- AXIOMS
prop_maxCount : maxCount > 0


