{-# OPTIONS --safe --without-K #-}

module Lib.Level where

open import Agda.Primitive public
open import Agda.Builtin.Nat

record Lift {a} ℓ (A : Set a) : Set (a ⊔ ℓ) where
  inductive
  constructor lift
  field lower : A

open Lift public

level : Nat → Level
level zero = lzero
level (suc n) = lsuc (level n)

levelOfType : ∀ {a} → Set a → Level
levelOfType {a} _ = a

levelOfTerm : ∀ {a} {A : Set a} → A → Level
levelOfTerm {a} _ = a
