structure Bounded /-SEES-/ (ctx:BoundedCtx) where
	count : Nat

namespace Bounded

@[simp]

@[simp]
 def Default : Bounded ctx := 
	{count := default }

instance: Machine BoundedCtx (Bounded ctx) where
	context := ctx
	invariant m := 
	default := Default

 def INITIALISATION: InitEvent (Bounded ctx) Unit Unit := 
	newInitEvent'' {
		 init _ := { count ≔ 0 }
		 safety _ := by sorry 
	}

def 

[def Incr.guard_grd (m : Bounded ctx) : Prop := 
	 m.count  < ctx. maxCount

def Incr.action (m : Bounded ctx) : Bounded ctx := 
	 { count ≔ count + 1 }

def Incr : OrdinaryEvent (Bounded ctx) Unit Unit := 
	 newEvent'' {
		guard m :=Incr.guard_grd m
		action m _ := Incr.action_count ≔ count + 1 m
		safety m := by sorry
},def Decr.guard_grd (m : Bounded ctx) : Prop := 
	 m.count > 0

def Decr.action (m : Bounded ctx) : Bounded ctx := 
	 { count ≔ count − 1 }

def Decr : OrdinaryEvent (Bounded ctx) Unit Unit := 
	 newEvent'' {
		guard m :=Decr.guard_grd m
		action m _ := Decr.action_count ≔ count − 1 m
		safety m := by sorry
},def Discard.guard_grd1 (m : Bounded ctx) : Prop := 
	 m.count > 0

def Discard.guard_grd2 (m : Bounded ctx) : Prop := 
	 m.k ≤ count

def Discard.guard_grd3 (m : Bounded ctx) : Prop := 
	 m. k > 0

def Discard.guard_grd_type (m : Bounded ctx) : Prop := 
	 m.k ∈ ℕ

def Discard.action (m : Bounded ctx) : Bounded ctx := 
	 { count ≔ count − k }

def Discard : OrdinaryEvent (Bounded ctx) Unit Unit := 
	 newEvent'' {
		guard m :=Discard.guard_grd1 m^Discard.guard_grd2 m^Discard.guard_grd3 m^Discard.guard_grd_type m
		action m _ := Discard.action_count ≔ count − k m
		safety m := by sorry
}]