{-# OPTIONS --safe --without-K #-}

module Lib.UnitOrEmpty.Type where

open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
open import Lib.Equality.Type

⊤or⊥ : Set₁
⊤or⊥ = Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
