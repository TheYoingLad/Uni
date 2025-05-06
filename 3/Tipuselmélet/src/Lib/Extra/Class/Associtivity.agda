{-# OPTIONS --safe #-}
module Lib.Extra.Class.Associtivity where

open import Lib.Equality using (_≡_)

record Assoc {i} (A : Set i) (_∘_ : A → A → A) : Set i where
    field
        ass∘ : (a b c : A) → (a ∘ b) ∘ c ≡ a ∘ (b ∘ c)