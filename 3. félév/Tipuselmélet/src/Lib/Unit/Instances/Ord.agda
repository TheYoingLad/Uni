{-# OPTIONS --safe --without-K #-}

module Lib.Unit.Instances.Ord where

open import Lib.Class.Ord
open import Lib.Unit.Type
open import Lib.Ordering
open import Lib.Unit.Instances.Eq
open import Lib.Equality.Type
open import Lib.Sigma.Type

instance
  Ord⊤ : Ord ⊤
  Ord.eq Ord⊤ = Eq⊤
  Ord.compare Ord⊤ _ _ = EQ
  Ord.flippable Ord⊤ = (λ ()) , λ ()
  Ord.equality Ord⊤ = refl
  Ord.consistencyWithEq Ord⊤ _ = refl
