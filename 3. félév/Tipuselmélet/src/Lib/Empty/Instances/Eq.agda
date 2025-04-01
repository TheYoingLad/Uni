{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Instances.Eq where

open import Lib.Class.Eq
open import Lib.Empty.Type
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Unit.Type

instance
  Eq⊥ : Eq ⊥
  Eq._≡ᵗ_ Eq⊥ _ _ = just refl , ⊤ , refl
  Eq.eqIsJust Eq⊥ _ = tt
