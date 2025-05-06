{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Instances where

open import Lib.Sum.Type
open import Lib.Sum.Instances.DecidableEquality public
open import Lib.Sum.Instances.Eq public
open import Lib.Sum.Instances.Ord public

-- Nagyon nem itt van ezek helye, de egyelőre nincs máshol logikus helye.
-- For the time being, ez itt marad!
instance
  inlᵢ : ∀{i j}{A : Set i}{B : Set j} → ⦃ a : A ⦄ → A ⊎ B
  inlᵢ ⦃ x ⦄ = inl x

  inrᵢ : ∀{i j}{A : Set i}{B : Set j} → ⦃ b : B ⦄ → A ⊎ B
  inrᵢ ⦃ x ⦄ = inr x
