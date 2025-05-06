{-# OPTIONS --safe --without-K #-}

module Lib.Reflects.Properties where

open import Lib.Equality
open import Lib.Reflects.Base
open import Lib.Bool.Type
open import Lib.Bool.Base
open import Lib.Empty.Base

¬-reflects : ∀{i}{P : Set i}{b : Bool} → Reflects P b → Reflects (¬ P) (not b)
¬-reflects (ofʸ  p) = ofⁿ (λ f → f p)
¬-reflects (ofⁿ ¬p) = ofʸ ¬p

------------------------------------------------------------------------

fromEquivalence : ∀{i}{P : Set i}{b : Bool} → (T b → P) → (P → T b) → Reflects P b
fromEquivalence {b = true}  sound complete = ofʸ (sound _)
fromEquivalence {b = false} sound complete = ofⁿ complete

det : ∀{i}{P : Set i}{b b′ : Bool} → Reflects P b → Reflects P b′ → b ≡ b′
det (ofʸ  p) (ofʸ  p′) = refl
det (ofʸ  p) (ofⁿ ¬p′) = exfalso (¬p′ p)
det (ofⁿ ¬p) (ofʸ  p′) = exfalso (¬p p′)
det (ofⁿ ¬p) (ofⁿ ¬p′) = refl
