{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Instances.Ord where

open import Lib.Nat.Type
open import Lib.Nat.Properties
open import Lib.Nat.Instances.Eq

open import Lib.Ordering.Type
open import Lib.Class.Eq
open import Lib.Class.Ord

open import Lib.Sigma.Type

open import Lib.Equality.Type
open import Lib.Equality.Base

open import Lib.Sum.Type
open import Lib.Sum.Base

open import Lib.Unit.Type

open import Lib.Maybe.Type
open import Lib.Maybe.Base

compareℕ : ℕ → ℕ → Ordering
compareℕ zero zero = EQ
compareℕ zero (suc b) = LT
compareℕ (suc a) zero = GT
compareℕ (suc a) (suc b) = compareℕ a b

flippableℕ : {x y : ℕ} → compareℕ x y ≡ LT ↔ compareℕ y x ≡ GT
flippableℕ {zero} {zero} = (λ ()) , (λ ())
flippableℕ {zero} {suc y} = (λ x → refl) , (λ x → refl)
flippableℕ {suc x} {zero} = (λ ()) , (λ ())
flippableℕ {suc x} {suc y} = flippableℕ {x} {y}

equalityℕ : {x : ℕ} → compareℕ x x ≡ EQ
equalityℕ {zero} = refl
equalityℕ {suc x} = equalityℕ {x}

consistencyWithEqℕ : {x y : ℕ} → x ≡ⁱ y → compareℕ x y ≡ EQ
consistencyWithEqℕ {zero} {zero} e = refl
consistencyWithEqℕ {suc x} {suc y} e with x ≡ᵗ y in eq1
... | just p , t , r = consistencyWithEqℕ {x} {y} (cast (trans r (sym (cong (λ a → fst (snd a)) eq1))) tt)

instance
  Ordℕ : Ord ℕ
  Ordℕ = OrdInstance ⦃ Eqℕ ⦄ compareℕ (λ {x} {y} → flippableℕ {x} {y}) (λ {x} → equalityℕ {x}) λ {x} {y} → consistencyWithEqℕ {x} {y}
  --                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  --                                  Agda stupid here...
