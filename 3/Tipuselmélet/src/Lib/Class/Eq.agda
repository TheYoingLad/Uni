{-# OPTIONS --safe --without-K #-}

module Lib.Class.Eq where

open import Agda.Primitive

open import Lib.Empty.Type
open import Lib.Empty.Base

open import Lib.Maybe.Type
open import Lib.Maybe.Base

open import Lib.Sigma.Type

open import Lib.Sum.Type

open import Lib.Equality

open import Lib.Unit.Type

open import Lib.Bool.Type

record Eq {i} (A : Set i) : Set (lsuc i) where
  constructor EqInstance
  inductive
  field
    _≡ᵗ_ : (a b : A) → Σ (Maybe (a ≡ b)) (λ x → Σ Set (IsJust x ≡_))
    eqIsJust : {a b : A} → a ≡ b → IsJust (fst (a ≡ᵗ b))

  _≡ⁱ_ : (a b : A) → Set
  a ≡ⁱ b = fst (snd (a ≡ᵗ b))

  _≡ₚᵣ_ : (a b : A) → Maybe (a ≡ b)
  a ≡ₚᵣ b = fst (a ≡ᵗ b)

  _==_ : (a b : A) → Bool
  a == b with a ≡ₚᵣ b
  ... | just _  = true
  ... | nothing = false

  _/=_ : (a b : A) → Bool
  a /= b with a ≡ₚᵣ b
  ... | just _  = false
  ... | nothing = true

  infix 4 _≡ᵗ_ _≡ⁱ_ _≡ₚᵣ_ _==_ _/=_

open Eq {{...}} public
