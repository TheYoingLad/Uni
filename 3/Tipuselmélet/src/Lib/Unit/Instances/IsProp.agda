{-# OPTIONS --safe --without-K #-}

module Lib.Unit.Instances.IsProp where

open import Lib.Unit.Type
open import Lib.Sigma.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Class.IsProp

instance
  IsProp⊤ : IsProp ⊤
  IsProp⊤ = isProp-proof λ x y → refl , J (λ _ → refl ≡_) refl
