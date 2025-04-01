{-# OPTIONS --safe --without-K #-}

module Lib.Containers.Vector.Type where

open import Lib.Nat.Type

infixr 5 _∷_
data Vec {i} (A : Set i) : ℕ → Set i where
  instance [] : Vec A 0
  _∷_ : {n : ℕ}(x : A)(xs : Vec A n) → Vec A (suc n)
