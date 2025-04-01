{-# OPTIONS --safe --without-K #-}

module Lib.Unit.Instances.IsSet where

open import Lib.Unit.Type
open import Lib.Sigma.Type
open import Lib.Equality.Type
open import Lib.Class.IsSet

instance
  IsSet⊤ : IsSet ⊤
  IsSet⊤ = isSet-proof λ {tt .tt refl refl → refl , λ {refl → refl}}
