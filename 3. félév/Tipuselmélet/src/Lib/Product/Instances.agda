{-# OPTIONS --safe --without-K #-}

module Lib.Product.Instances where

open import Lib.Product.Instances.DecidableEquality public
open import Lib.Product.Instances.Eq public
open import Lib.Product.Instances.Ord public

open import Lib.Product.Type

-- Ennek most ennél nincs jobb helye, addig itt marad.
instance
  A×B : ∀{i j}{A : Set i}{B : Set j} → ⦃ A ⦄ → ⦃ B ⦄ → A × B
  A×B ⦃ a ⦄ ⦃ b ⦄ = a , b
