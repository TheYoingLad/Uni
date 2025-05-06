{-# OPTIONS --safe --without-K #-}

module Lib.TruncLevel.Literals where

open import Agda.Builtin.FromNat public
open import Agda.Builtin.FromNeg public
open import Lib.Nat.Type
open import Lib.TruncLevel.Type
open import Lib.TruncLevel.Base
open import Lib.Unit.Type public
open import Lib.Empty.Type
open Number
open Negative

instance
  NumTruncLevel : Number TruncLevel
  Constraint NumTruncLevel n = ⊤
  fromNat NumTruncLevel n ⦃ i ⦄ = toℕ₋₂⟨ n ⟩

  NegTruncLevel : Negative TruncLevel
  Constraint NegTruncLevel zero = ⊤
  Constraint NegTruncLevel (suc zero) = ⊤
  Constraint NegTruncLevel (suc (suc zero)) = ⊤
  Constraint NegTruncLevel (suc (suc (suc n))) = ⊥
  fromNeg NegTruncLevel zero = ⟨0⟩
  fromNeg NegTruncLevel (suc zero) = ⟨-1⟩
  fromNeg NegTruncLevel (suc (suc zero)) = ⟨-2⟩
