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


-- MACHINE 
structure  m1/-SEES-/ (ctx:cd) where
	a:Nat
	b:Nat
	c:Nat

namespace m1


@[simp]
def inv1 (m : m1 ctx) : Prop :=
	m.a∈Nat
@[simp]
def inv2 (m : m1 ctx) : Prop :=
	m.b∈Nat
@[simp]
def inv3 (m : m1 ctx) : Prop :=
	m.c∈Nat
@[simp]
def inv4 (m : m1 ctx) : Prop :=
	m.n=m.a+m.b+m.c
@[simp]
def inv5 (m : m1 ctx) : Prop :=
	m.a=0∨m.c=0
@[simp]
def thm1 (m : m1 ctx) : Prop :=
	m.a+m.b+m.c∈Nat
@[simp]
def thm2 (m : m1 ctx) : Prop :=
	m.c>0∨m.a>0∨m.a+m.b<m.d∧m.c=0∨0<m.b∧m.a=0

@[simp]
def  (m : m1 ctx) : Prop :=
	2
@[simp]
def Default : m1ctx :=
	{
	c := default
	b := default
	a := default}

instance: Machine COLOR (m1 ctx) where
  context := ctx
  invariant m := thm2 ∧ thm1 ∧ inv5 ∧ inv4 ∧ inv3 ∧ inv2 ∧ inv1
  variant m := 
  default := Default

def Initialisation : InitEvent (m1 ctx) Unit Unit :=
	newInitEvent'' {
		 init _ :={
			c≔0
			b≔0
			a≔0 }
		safety _ := by sorry 
	}




@[simp]
def ML_out.grd1 (m : m1 ctx) : Prop :=
	m.a+m.b<m.d

@[simp]
def ML_out.grd2 (m : m1 ctx) : Prop :=
	m.c=0

@[simp]
def ML_out.action (m : m1 ctx) : m1 ctx :=
	{
	m.a≔m.a+1}

def ML_out: OrdinaryEvent (m1 ctx) Unit Unit :=
	newEvent'' {
		guard m :=ML_out.grd2 m ∧ ML_out.grd1 m
		action m _ := ML_out.action m
		safety m := by sorry
	}


@[simp]
def IL_in.grd1 (m : m1 ctx) : Prop :=
	m.a>0

@[simp]
def IL_in.action (m : m1 ctx) : m1 ctx :=
	{
	m.b≔m.b+1
	m.a≔m.a−1}

def IL_in: OrdinaryEvent (m1 ctx) Unit Unit :=
	newEvent'' {
		guard m :=IL_in.grd1 m
		action m _ := IL_in.action m
		safety m := by sorry
	}


@[simp]
def IL_out.grd1 (m : m1 ctx) : Prop :=
	0<m.b

@[simp]
def IL_out.grd2 (m : m1 ctx) : Prop :=
	m.a=0

@[simp]
def IL_out.action (m : m1 ctx) : m1 ctx :=
	{
	m.c≔m.c+1
	m.b≔m.b−1}

def IL_out: OrdinaryEvent (m1 ctx) Unit Unit :=
	newEvent'' {
		guard m :=IL_out.grd2 m ∧ IL_out.grd1 m
		action m _ := IL_out.action m
		safety m := by sorry
	}


@[simp]
def ML_in.grd1 (m : m1 ctx) : Prop :=
	m.c>0

@[simp]
def ML_in.action (m : m1 ctx) : m1 ctx :=
	{
	m.c≔m.c−1}

def ML_in: OrdinaryEvent (m1 ctx) Unit Unit :=
	newEvent'' {
		guard m :=ML_in.grd1 m
		action m _ := ML_in.action m
		safety m := by sorry
	}