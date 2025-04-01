{-# OPTIONS --safe --without-K #-}

module Lib.Isomorphism.Type where

open import Agda.Primitive
open import Lib.Sigma.Type
open import Lib.Equality.Type

-- \~= : ≅
infix 0 _≅_
record _≅_ {i j}(A : Set i)(B : Set j) : Set (i ⊔ j) where
  constructor ≅-proof
  field
    coe→ : A → B
    coe← : B → A
    id←→ : ∀ a → coe← (coe→ a) ≡ a -- funexted version
    id→← : ∀ b → coe→ (coe← b) ≡ b

  equivalence : A ↔ B
  equivalence = coe→ , coe←

open _≅_ public
