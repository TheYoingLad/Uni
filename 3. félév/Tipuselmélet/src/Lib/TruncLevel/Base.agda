{-# OPTIONS --safe --without-K #-}

module Lib.TruncLevel.Base where

open import Lib.Nat.Type
open import Lib.TruncLevel.Type

toTruncLevel⟨_⟩₋₂ : ℕ → ℕ₋₂
toTruncLevel⟨ zero ⟩₋₂ = ⟨-2⟩
toTruncLevel⟨ suc n ⟩₋₂ = suc toTruncLevel⟨ n ⟩₋₂

toTruncLevel⟨_⟩₋₁ : ℕ → ℕ₋₂
toTruncLevel⟨ n ⟩₋₁ = suc toTruncLevel⟨ n ⟩₋₂

toTruncLevel⟨_⟩ : ℕ → ℕ₋₂
toTruncLevel⟨ n ⟩ = suc (suc toTruncLevel⟨ n ⟩₋₂)

toℕ₋₂⟨_⟩₋₂ = toTruncLevel⟨_⟩₋₂
toℕ₋₂⟨_⟩₋₁ = toTruncLevel⟨_⟩₋₁
toℕ₋₂⟨_⟩   = toTruncLevel⟨_⟩

fromTruncLevel⟨_⟩₊₂ : ℕ₋₂ → ℕ
fromTruncLevel⟨ ⟨-2⟩  ⟩₊₂ = zero
fromTruncLevel⟨ suc n ⟩₊₂ = suc fromTruncLevel⟨ n ⟩₊₂

fromTruncLevel⟨_⟩₊₁ : ℕ₋₂ → ℕ
fromTruncLevel⟨ n ⟩₊₁ = suc fromTruncLevel⟨ n ⟩₊₂

fromTruncLevel⟨_⟩ : ℕ₋₂ → ℕ
fromTruncLevel⟨ n ⟩ = suc (suc fromTruncLevel⟨ n ⟩₊₂)

fromℕ₋₂⟨_⟩₊₂ = fromTruncLevel⟨_⟩₊₂
fromℕ₋₂⟨_⟩₊₁ = fromTruncLevel⟨_⟩₊₁
fromℕ₋₂⟨_⟩ = fromTruncLevel⟨_⟩

raiseLevel : ℕ → TruncLevel → TruncLevel
raiseLevel zero k = k
raiseLevel (suc n) k = suc (raiseLevel n k)
