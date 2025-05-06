{-# OPTIONS --safe --without-K #-}

module Lib.Ordering.Instances.Ord where

open import Lib.Class.Ord
open import Lib.Ordering.Instances.Eq
open import Lib.Ordering.Type

open import Lib.Sigma.Type
open import Lib.Equality

instance
  OrdOrdering : Ord Ordering
  Ord.eq OrdOrdering = EqOrdering
  Ord.compare OrdOrdering LT LT = EQ
  Ord.compare OrdOrdering LT EQ = LT
  Ord.compare OrdOrdering LT GT = LT
  Ord.compare OrdOrdering EQ LT = GT
  Ord.compare OrdOrdering EQ EQ = EQ
  Ord.compare OrdOrdering EQ GT = LT
  Ord.compare OrdOrdering GT LT = GT
  Ord.compare OrdOrdering GT EQ = GT
  Ord.compare OrdOrdering GT GT = EQ
  Ord.flippable OrdOrdering {LT} {LT} = (λ ()) , (λ ())
  Ord.flippable OrdOrdering {LT} {EQ} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable OrdOrdering {LT} {GT} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable OrdOrdering {EQ} {LT} = sym , sym
  Ord.flippable OrdOrdering {EQ} {EQ} = (λ ()) , (λ ())
  Ord.flippable OrdOrdering {EQ} {GT} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable OrdOrdering {GT} {LT} = sym , sym
  Ord.flippable OrdOrdering {GT} {EQ} = sym , sym
  Ord.flippable OrdOrdering {GT} {GT} = (λ ()) , (λ ())
  Ord.equality OrdOrdering {LT} = refl
  Ord.equality OrdOrdering {EQ} = refl
  Ord.equality OrdOrdering {GT} = refl
  Ord.consistencyWithEq OrdOrdering {LT} {LT} e = refl
  Ord.consistencyWithEq OrdOrdering {EQ} {EQ} e = refl
  Ord.consistencyWithEq OrdOrdering {GT} {GT} e = refl
