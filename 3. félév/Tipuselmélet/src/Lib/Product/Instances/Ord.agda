{-# OPTIONS --safe --without-K #-}

module Lib.Product.Instances.Ord where

open import Lib.Product.Type
open import Lib.Product.Properties
open import Lib.Class.Eq
open import Lib.Class.Ord
open import Lib.Ordering.Type
open import Lib.Ordering.Base
open import Lib.Ordering.Properties
open import Lib.Product.Instances.Eq
import Lib.Sigma.Type as Σ
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
  ... | LT | LT = (λ _ → trans (sym eq2) (Σ.fst (flippable ⦃ ordA ⦄ {a1} {a2}) eq1)) Σ., λ _ → refl
  ... | LT | EQ = (λ _ → exfalso (EQ≢GT (trans (sym eq2) (Σ.fst (flippable {x = a1} {a2}) eq1)))) Σ., λ _ → refl
  ... | LT | GT = (λ _ → refl) Σ., (λ _ → refl)
  ... | EQ | LT = (λ _ → exfalso (EQ≢GT (trans (sym eq1) (Σ.fst (flippable {x = a2} {a1}) eq2)))) Σ., λ ()
  ... | EQ | EQ = flippable ⦃ ordB ⦄ {b1} {b2}
  ... | EQ | GT = (λ _ → refl) Σ., λ _ → exfalso (EQ≢LT (trans (sym eq1) (Σ.snd (flippable {x = a1} {a2}) eq2)))
  ... | GT | LT = sym Σ., sym
  ... | GT | EQ = (λ ()) Σ., λ _ → exfalso (EQ≢LT (trans (sym eq2) (Σ.snd (flippable {x = a2} {a1}) eq1)))
  ... | GT | GT = (λ _ → refl) Σ., λ _ → trans (sym eq1) (Σ.snd (flippable {x = a1} {a2}) eq2)
  Ord.equality (Ord× ⦃ ordA ⦄ ⦃ ordB ⦄) {a , b} with compare a a in eq1
  ... | LT = trans (sym eq1) (equality {x = a})
  ... | GT = trans (sym eq1) (equality {x = a})
  ... | EQ = equality {x = b}
  Ord.consistencyWithEq (Ord× ⦃ ordA ⦄ ⦃ ordB ⦄) {a1 , b1} {a2 , b2} e with compare a1 a2 in eq1 | a1 ≡ᵗ a2 in eq2
  ... | LT | just eq3 Σ., _ Σ., refl = trans (sym eq1) (consistencyWithEq ⦃ ordA ⦄ {a1} {a2} (cast (sym (cong (λ (a Σ., b Σ., c) → b) eq2)) tt))
  ... | LT | nothing Σ., _ Σ., refl = exfalso e
  ... | GT | just eq3 Σ., _ Σ., refl = trans (sym eq1) (consistencyWithEq ⦃ ordA ⦄ {a1} {a2} (cast (sym (cong (λ (a Σ., b Σ., c) → b) eq2)) tt))
  ... | GT | nothing Σ., _ Σ., refl = exfalso e
  ... | EQ | nothing Σ., _ Σ., refl = exfalso e
  ... | EQ | just eq3 Σ., _ Σ., refl with compare b1 b2 in eq4 | b1 ≡ᵗ b2 in eq5
  ... | LT | nothing Σ., _ Σ., refl = exfalso e
  ... | LT | just eq6 Σ., _ Σ., refl = trans (sym eq4) (consistencyWithEq ⦃ ordB ⦄ {b1} {b2} (cast (sym (cong (λ (a Σ., b Σ., c) → b) eq5)) tt))
  ... | EQ | _ = refl
  ... | GT | nothing Σ., _ Σ., refl = exfalso e
  ... | GT | just eq6 Σ., _ Σ., refl = trans (sym eq4) (consistencyWithEq ⦃ ordB ⦄ {b1} {b2} (cast (sym (cong (λ (a Σ., b Σ., c) → b) eq5)) tt))
