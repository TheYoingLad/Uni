{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Instances.IsSet where

open import Lib.Empty.Type
open import Lib.Class.IsSet

instance
  IsSet⊥ : IsSet ⊥
  IsSet⊥ = isSet-proof (λ ())
