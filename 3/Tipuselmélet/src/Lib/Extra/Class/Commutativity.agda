{-# OPTIONS --safe #-}
module Lib.Extra.Class.Commutativity where

open import Lib.Equality using (_≡_)

record Commutativity {i} (A : Set i) (_∘_ : A → A → A) : Set i where
    field
        comm∘ : (a b : A) → a ∘ b ≡ b ∘ a