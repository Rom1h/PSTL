{-# LANGUAGE OverloadedStrings #-}

module DataExprSpec where

import Test.Hspec
import qualified Data.Text as T
import LeanMachineAst.DataExpr


dataExprTest :: Spec
dataExprTest = do
  describe "Test textToType" $ do
    it "text maxCount ∈ ℕ should be type Nat" $ do
      textToType "maxCount ∈ ℕ" `shouldBe` Nat
      
  describe "Test Tokenize" $ do

    it "tokenize on string 'a+1< a+2 ∨ bob> a'" $ do
      tokenize "a+1< a+2 ∨ bob> a"
        `shouldBe` map T.pack ["a", "+", "1", "<", "a", "+", "2", "∨", "bob", ">", "a"]
    
    it "tokenize on string 'a+1< a+2 ∨ bob> a'" $ do
      tokenize "a_b+1< a+2 ∨ bob> a"
        `shouldBe` map T.pack ["a_b", "+", "1", "<", "a", "+", "2", "∨", "bob", ">", "a"]
    it "tokenize on string '(a+b=3) ∧ (c<d)'" $ do
      tokenize "(a+b=3) ∧ (c<d)"
        `shouldBe` map T.pack ["(","a", "+", "b", "=", "3",")", "∧", "(", "c", "<", "d", ")"]
  
  describe "Test textToExpr" $ do

    it "textToExprs simple addition with comparison" $ do
      textToExpr "a+b<d"
        `shouldBe` Inf (Add (Cons "a") (Cons "b")) (Cons "d")

    it "textToExprs logical expression with ∧ and ∨" $ do
      textToExpr "a∧2"
        `shouldBe` And (Cons "a") (Cons "2")

    it "textToExprs logical expression with ∧ and ∨" $ do
      textToExpr "a=1 ∧ b=2 ∨ c=3"
        `shouldBe` Or (And (Eq (Cons "a") (Cons "1")) (Eq (Cons "b") (Cons "2"))) (Eq (Cons "c") (Cons "3"))

    it "textToExprs implication expression" $ do
      textToExpr "a=1 ⇒ b=2"
        `shouldBe` Impliq (Eq (Cons "a") (Cons "1")) (Eq (Cons "b") (Cons "2"))

    it "respects parentheses in complex expressions" $ do
      textToExpr "(a+b=3 ∧ c<d) ∨ a=b"
        `shouldBe` Or (And (Eq (Add (Cons "a") (Cons "b")) (Cons "3")) (Inf (Cons "c") (Cons "d"))) (Eq (Cons "a") (Cons "b"))
    it "multiple parenthesis test" $ do
      textToExpr "(a+b=3 ∧ c<d) ∨ (a=b) ∨ (a=b) ∨ (c=d) "
        `shouldBe` Or (And (Eq (Add (Cons "a") (Cons "b")) (Cons "3")) (Inf (Cons "c") (Cons "d"))) (Or (Eq (Cons "a") (Cons "b")) (Or (Eq (Cons "a") (Cons "b")) (Eq (Cons "c") (Cons "d"))))
    
    it "respects parentheses in simple expressions" $ do
      textToExpr "(a=1) < 3"
        `shouldBe`  Inf (Eq (Cons "a") (Cons "1")) (Cons "3")
    it "(ml_tl=green ∧ a+b+1<d) ∨ (ml_tl=green ∧ a+b+1=d) ∨ (il_tl=green ∧ b>1) ∨ (il_tl=green ∧ b=1) ∨ (ml_tl=red ∧ a+b<d ∧ c=0 ∧ il_pass=1) ∨ (il_tl=red ∧ 0<b ∧ a=0 ∧ ml_pass=1) " $ do
      textToExpr "(ml_tl=green ∧ a+b+1<d) ∨\n(ml_tl=green ∧ a+b+1=d) ∨ \n(il_tl=green ∧ b>1) ∨\n(il_tl=green ∧ b=1) ∨ \n(ml_tl=red ∧ a+b<d ∧ c=0 ∧ il_pass=1) ∨\n(il_tl=red ∧ 0<b ∧ a=0 ∧ ml_pass=1) ∨\n0<a ∨\n0<c"
        `shouldBe`  Inf (Eq (Cons "a") (Cons "1")) (Cons "3")


    it "maxCount ∈ ℕ" $ do
      textToExpr "maxCount ∈ ℕ"
        `shouldBe`  In (Cons "maxCount") Nat
    
    it "count ≔ 0" $ do
      textToExpr "count &lt; maxCount"
        `shouldBe`  In (Cons "maxCount") Nat
   
  describe "Test getType" $ do

    it "getType should return Nat for variable 'x' in invariant 'x ∈ ℕ'" $ do
      let invs = [Invariant "test" "x ∈ ℕ"]
      getType "x" invs `shouldBe` Nat
    
        
