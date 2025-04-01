{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Instances.IsProp where

open import Lib.Empty.Type
open import Lib.Class.IsProp

instance
  IsProp⊥ : IsProp ⊥
  IsProp⊥ = isProp-proof (λ ())
