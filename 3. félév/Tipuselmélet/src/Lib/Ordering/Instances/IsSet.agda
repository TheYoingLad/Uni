{-# OPTIONS --safe --without-K #-}

module Lib.Ordering.Instances.IsSet where

open import Lib.Ordering.Type
open import Lib.Ordering.Properties
open import Lib.Class.IsSet
open import Lib.Ordering.Instances.DecidableEquality
open import Lib.Dec.Type
open import Lib.Dec.InstanceGenerators.IsSet

instance
  IsSetOrdering : IsSet Ordering
  IsSetOrdering = DecidableEquality→IsSet (DecProof _≟_)
