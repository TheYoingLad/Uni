{-# OPTIONS --safe --without-K #-}

module Lib.Equality.Type where

open import Agda.Builtin.Equality public
open import Lib.Empty.Type

-- \==n == ≢
infix 4 _≢_
_≢_ : ∀{i}{A : Set i} → A → A → Set i
x ≢ y = x ≡ y → ⊥
