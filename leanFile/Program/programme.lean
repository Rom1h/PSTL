import LeanMachines.Event.Basic
import LeanMachines.Event.Ordinary
import LeanMachines.Event.Convergent
import LeanMachines.NonDet.Ordinary

-- CONTEXT
structure testContext where
	maxCount : None
	prop_maxCount : maxCount>0


-- MACHINE 
structure  m0/-SEES-/ (ctx:cd) where
	n:Nat

namespace m0


@[simp]
def inv1 (m : m0 ctx) : Prop :=
	m.nÔêêÔäò
@[simp]
def inv2 (m : m0 ctx) : Prop :=
	m.nÔëñd
@[simp]
def thm1 (m : m0 ctx) : Prop :=
	m.n>0

@[simp]
def Default : m0ctx :=
	{
	n := default}

instance: Machine m0 (m0 ctx) where
  context := ctx
  invariant m := thm1 ∧ inv2 ∧ inv1
  default := Default

def Initialisation : InitEvent (m0 ctx) Unit Unit :=
	newInitEvent'' {
		 init _ :={
			nÔëö0 }
		safety _ := by sorry 
	}




@[simp]
def ML_out.grd1 (m : m0 ctx) : Prop :=
	m.n<m.d

@[simp]
def ML_out.action (m : m0 ctx) : m0 ctx :=
	{
	m.nÔëön+1}

def ML_out: OrdinaryEvent (m0 ctx) Unit Unit :=
	newEvent'' {
		guard m :=ML_out.grd1 m
		action m _ := ML_out.action m
		safety m := by sorry
	}


@[simp]
def ML_in.grd1 (m : m0 ctx) : Prop :=
	m.n>0

@[simp]
def ML_in.action (m : m0 ctx) : m0 ctx :=
	{
	m.nÔëönÔêÆ1}

def ML_in: OrdinaryEvent (m0 ctx) Unit Unit :=
	newEvent'' {
		guard m :=ML_in.grd1 m
		action m _ := ML_in.action m
		safety m := by sorry
	}