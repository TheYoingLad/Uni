{-# OPTIONS --safe --without-K #-}

module Lib.Product.Instances.DecidableEquality where

open import Lib.Product.Properties
open import Lib.Product.Type
open import Lib.Dec
open import Lib.Dec.PatternSynonym
open import Lib.Sum.Base

instance
  DecEq× : ∀{i j}{A : Set i}{B : Set j} → ⦃ DecidableEquality A ⦄ → ⦃ DecidableEquality B ⦄ → DecidableEquality (A × B)
  DecEq× ⦃ i1 ⦄ ⦃ i2 ⦄ = DecProof λ (x1 , x2) (y1 , y2) →
    case (dec× (decide i1 x1 y1) (decide i2 x2 y2))
         (λ e → yes (snd ×≡ e))
         (λ e → no λ e2 → e (fst ×≡ e2))
