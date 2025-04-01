{-# OPTIONS --safe --without-K #-}

module Lib.Unit.Instances.IsContr where

open import Lib.Unit.Type
open import Lib.Sigma.Type
open import Lib.Equality.Type
open import Lib.Class.IsContr

instance
  IsContr⊤ : IsContr ⊤
  IsContr⊤ = isContr-proof (tt , (λ _ → refl))
