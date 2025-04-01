{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Instances where

open import Lib.Sigma.Instances.DecidableEquality public
open import Lib.Sigma.Instances.Eq public
open import Lib.Sigma.Instances.Ord public

-- Ennek egyelőre nincs jobb helye, itt marad!
open import Lib.Sigma.Type

instance
  ΣAB : ∀{i j}{A : Set i}{B : A → Set j} → ⦃ a : A ⦄ → ⦃ B a ⦄ → Σ A B
  ΣAB ⦃ a ⦄ ⦃ b ⦄ = a , b
