{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Instances.Ord where

open import Lib.Empty.Instances.Eq

open import Lib.Class.Ord
open import Lib.Ordering

open import Lib.Empty.Type

open import Lib.Sigma.Type

open import Lib.Equality.Type

instance
  Ord⊥ : Ord ⊥
  Ord.eq Ord⊥ = Eq⊥
  Ord.compare Ord⊥ _ _ = EQ
  Ord.flippable Ord⊥ = (λ ()) , (λ ())
  Ord.equality Ord⊥ = refl
  Ord.consistencyWithEq Ord⊥ _ = refl
