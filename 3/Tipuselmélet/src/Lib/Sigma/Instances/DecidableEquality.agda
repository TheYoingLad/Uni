{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Instances.DecidableEquality where

open import Lib.Sigma.Type
open import Lib.Sigma.Properties
open import Lib.Dec

instance
  DecEqΣ : ∀{i j}{A : Set i}{B : A → Set j} 
         → ⦃ DecidableEquality A ⦄
         → ⦃ ∀{a} → DecidableEquality (B a) ⦄
         → DecidableEquality (Σ A B)
  DecEqΣ ⦃ i1 ⦄ ⦃ i2 ⦄ = DecProof (Σ-dec (decide i1) (decide i2))


  DecEq× : ∀{i j}{A : Set i}{B : Set j} → ⦃ DecidableEquality A ⦄ → ⦃ DecidableEquality B ⦄ → DecidableEquality (A × B)
  DecEq× ⦃ i1 ⦄ ⦃ i2 ⦄ = DecProof λ (x1 , x2) (y1 , y2) → ,-dec (decide i1 x1 y1) (decide i2 x2 y2)
