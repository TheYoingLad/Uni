{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Instances.Ord where

open import Lib.Fin.Type
open import Lib.Class.Eq
open import Lib.Class.Ord
open import Lib.Ordering.Type

open import Lib.Fin.Instances.Eq
open import Lib.Fin.Properties

open import Lib.Equality
open import Lib.Sigma.Type
open import Lib.Unit.Type

open import Lib.Sum.Type
open import Lib.Sum.Base
open import Lib.Maybe.Type
open import Lib.Maybe.Base

instance
  OrdFin : ∀{n} → Ord (Fin n)
  Ord.eq OrdFin = EqFin
  Ord.compare OrdFin fzero fzero = EQ
  Ord.compare OrdFin fzero (fsuc b) = LT
  Ord.compare OrdFin (fsuc a) fzero = GT
  Ord.compare OrdFin (fsuc a) (fsuc b) = compare a b
  Ord.flippable OrdFin {fzero} {fzero} = (λ ()) , (λ ())
  Ord.flippable OrdFin {fzero} {fsuc y} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable OrdFin {fsuc x} {fzero} = sym , sym
  Ord.flippable OrdFin {fsuc x} {fsuc y} = flippable ⦃ OrdFin ⦄ {x} {y}
  Ord.equality OrdFin {fzero} = refl
  Ord.equality OrdFin {fsuc x} = equality ⦃ OrdFin ⦄ {x}
  Ord.consistencyWithEq OrdFin {fzero} {fzero} _ = refl
  Ord.consistencyWithEq OrdFin {fsuc x} {fsuc y} e with x ≡ᵗ y in eq1
  ... | just p , t , r = Ord.consistencyWithEq OrdFin {x} {y} (cast (trans r (sym (cong (λ a → fst (snd a)) eq1))) tt)
