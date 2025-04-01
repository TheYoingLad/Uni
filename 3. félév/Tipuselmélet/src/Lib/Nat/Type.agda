{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Type where

open import Agda.Builtin.Nat
  using (suc ; zero)
  renaming (Nat to ℕ)
  public
