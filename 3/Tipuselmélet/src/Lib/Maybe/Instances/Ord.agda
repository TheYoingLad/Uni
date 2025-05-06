{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Instances.Ord where

open import Lib.Maybe.Type
open import Lib.Class.Eq
open import Lib.Class.Ord

open import Lib.Maybe.Instances.Eq
open import Lib.Ordering.Type

open import Lib.Sigma.Type
open import Lib.Equality

open import Lib.Unit.Type
open import Lib.Empty.Base

instance
  OrdMaybe : ∀{i}{A : Set i} → ⦃ Ord A ⦄ → Ord (Maybe A)
  Ord.eq (OrdMaybe ⦃ ordA ⦄) = EqMaybe
  Ord.compare (OrdMaybe ⦃ ordA ⦄) (just x) (just y) = compare x y
  Ord.compare (OrdMaybe ⦃ ordA ⦄) (just x) nothing = GT
  Ord.compare (OrdMaybe ⦃ ordA ⦄) nothing (just x) = LT
  Ord.compare (OrdMaybe ⦃ ordA ⦄) nothing nothing = EQ
  Ord.flippable (OrdMaybe ⦃ ordA ⦄) {just x} {just y} = flippable ⦃ ordA ⦄ {x} {y}
  Ord.flippable (OrdMaybe ⦃ ordA ⦄) {just x} {nothing} = sym , sym
  Ord.flippable (OrdMaybe ⦃ ordA ⦄) {nothing} {just x} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable (OrdMaybe ⦃ ordA ⦄) {nothing} {nothing} = (λ ()) , (λ ())
  Ord.equality (OrdMaybe ⦃ ordA ⦄) {just x} = equality ⦃ ordA ⦄ {x}
  Ord.equality (OrdMaybe ⦃ ordA ⦄) {nothing} = refl
  Ord.consistencyWithEq (OrdMaybe ⦃ ordA ⦄) {just x} {just y} e with x ≡ᵗ y in eq1
  ... | just eq2 , _ , refl = consistencyWithEq ⦃ ordA ⦄ {x} {y} (cast (cong (λ (a , b , c) → b) (sym eq1)) tt)
  ... | nothing , _ , refl = exfalso e
  Ord.consistencyWithEq (OrdMaybe ⦃ ordA ⦄) {nothing} {nothing} _ = refl
