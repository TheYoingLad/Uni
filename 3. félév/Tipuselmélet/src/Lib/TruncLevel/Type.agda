{-# OPTIONS --safe --without-K #-}

module Lib.TruncLevel.Type where

data TruncLevel : Set where
  ⟨-2⟩ : TruncLevel
  suc  : TruncLevel → TruncLevel

ℕ₋₂ = TruncLevel

pattern ⟨-1⟩ = suc ⟨-2⟩
pattern ⟨0⟩ = suc (suc ⟨-2⟩)
