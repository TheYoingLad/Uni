{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Type where

open import Lib.Nat.Type

data Fin : ℕ → Set where
  fzero : {n : ℕ} → Fin (suc n)
  fsuc  : {n : ℕ}(i : Fin n) → Fin (suc n)
