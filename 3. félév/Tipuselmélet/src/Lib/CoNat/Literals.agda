{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoNat.Literals where

open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.Unit.Type public
open import Agda.Builtin.FromNat public

instance
  Numℕ∞ : Number ℕ∞
  Number.Constraint Numℕ∞ _ = ⊤
  Number.fromNat Numℕ∞ n = embed n
