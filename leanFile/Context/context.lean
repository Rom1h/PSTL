import LeanMachines.Event.Basic
import LeanMachines.Event.Ordinary
import LeanMachines.Event.Convergent
import LeanMachines.NonDet.Ordinary

-- CONTEXT
structure COLOR where
	red : Nat
	green : Nat
	axm3 : green≠red
	axm4 : Color={green,red}
