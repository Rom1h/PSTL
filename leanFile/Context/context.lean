import LeanMachines.Event.Basic
import LeanMachines.Event.Ordinary
import LeanMachines.Event.Convergent
import LeanMachines.NonDet.Ordinary

-- CONTEXT
structure SENSOR where
	off : Nat
	on : Nat
	axm2 : ¬on=off
	axm1 : Sensor={on,off}
