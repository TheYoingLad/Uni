{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Instances.Ord where

open import Lib.Sigma.Type
open import Lib.Sigma.Properties
open import Lib.Sigma.Instances.Eq
open import Lib.Class.Eq
open import Lib.Class.Ord
open import Lib.Ordering.Type
open import Lib.Ordering.Base
open import Lib.Ordering.Properties

open import Lib.Equality

open import Lib.Empty.Type
open import Lib.Empty.Base
open import Lib.Unit.Type

open import Lib.Maybe.Type
open import Lib.Maybe.Base

instance
  Ord× : ∀{i j}{A : Set i}{B : Set j} → ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → Ord (A × B)
  Ord.eq Ord× = Eq×
  Ord.compare (Ord× ⦃ ordA ⦄ ⦃ ordB ⦄) (a1 , b1) (a2 , b2) with compare a1 a2
  ... | LT = LT
  ... | GT = GT
  ... | EQ = compare b1 b2
  Ord.flippable (Ord× ⦃ ordA ⦄ ⦃ ordB ⦄) {a1 , b1} {a2 , b2} with compare a1 a2 in eq1 | compare a2 a1 in eq2
  ... | LT | LT = (λ _ → trans (sym eq2) (fst (flippable ⦃ ordA ⦄ {a1} {a2}) eq1)) , λ _ → refl
  ... | LT | EQ = (λ _ → exfalso (EQ≢GT (trans (sym eq2) (fst (flippable {x = a1} {a2}) eq1)))) , λ _ → refl
  ... | LT | GT = (λ _ → refl) , (λ _ → refl)
  ... | EQ | LT = (λ _ → exfalso (EQ≢GT (trans (sym eq1) (fst (flippable {x = a2} {a1}) eq2)))) , λ ()
  ... | EQ | EQ = flippable ⦃ ordB ⦄ {b1} {b2}
  ... | EQ | GT = (λ _ → refl) , λ _ → exfalso (EQ≢LT (trans (sym eq1) (snd (flippable {x = a1} {a2}) eq2)))
  ... | GT | LT = sym , sym
  ... | GT | EQ = (λ ()) , λ _ → exfalso (EQ≢LT (trans (sym eq2) (snd (flippable {x = a2} {a1}) eq1)))
  ... | GT | GT = (λ _ → refl) , λ _ → trans (sym eq1) (snd (flippable {x = a1} {a2}) eq2)
  Ord.equality (Ord× ⦃ ordA ⦄ ⦃ ordB ⦄) {a , b} with compare a a in eq1
  ... | LT = trans (sym eq1) (equality {x = a})
  ... | GT = trans (sym eq1) (equality {x = a})
  ... | EQ = equality {x = b}
  Ord.consistencyWithEq (Ord× ⦃ ordA ⦄ ⦃ ordB ⦄) {a1 , b1} {a2 , b2} e with compare a1 a2 in eq1 | a1 ≡ᵗ a2 in eq2
  ... | LT | just eq3 , _ , refl = trans (sym eq1) (consistencyWithEq ⦃ ordA ⦄ {a1} {a2} (cast (sym (cong (λ (a , b , c) → b) eq2)) tt))
  ... | LT | nothing , _ , refl = exfalso e
  ... | GT | just eq3 , _ , refl = trans (sym eq1) (consistencyWithEq ⦃ ordA ⦄ {a1} {a2} (cast (sym (cong (λ (a , b , c) → b) eq2)) tt))
  ... | GT | nothing , _ , refl = exfalso e
  ... | EQ | nothing , _ , refl = exfalso e
  ... | EQ | just eq3 , _ , refl with compare b1 b2 in eq4 | b1 ≡ᵗ b2 in eq5
  ... | LT | nothing , _ , refl = exfalso e
  ... | LT | just eq6 , _ , refl = trans (sym eq4) (consistencyWithEq ⦃ ordB ⦄ {b1} {b2} (cast (sym (cong (λ (a , b , c) → b) eq5)) tt))
  ... | EQ | _ = refl
  ... | GT | nothing , _ , refl = exfalso e
  ... | GT | just eq6 , _ , refl = trans (sym eq4) (consistencyWithEq ⦃ ordB ⦄ {b1} {b2} (cast (sym (cong (λ (a , b , c) → b) eq5)) tt))

  -- For OrdΣ UIP is needed!!
