{-# OPTIONS --no-exact-split --safe --without-K #-}

module Lib.Nat.Literals where

open import Lib.Nat.Type
open import Lib.Unit.Type public
open import Agda.Builtin.FromNat public

instance
  NumNat : Number ℕ
  NumNat .Number.Constraint _ = ⊤
  NumNat .Number.fromNat m = m
