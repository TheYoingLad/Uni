{-# OPTIONS --safe --without-K #-}

module Lib.Ordering.Instances.DecidableEquality where

open import Lib.Dec
open import Lib.Ordering.Type
open import Lib.Ordering.Properties

instance
  DecEqOrdering : DecidableEquality Ordering
  DecEqOrdering = DecProof _≟_
