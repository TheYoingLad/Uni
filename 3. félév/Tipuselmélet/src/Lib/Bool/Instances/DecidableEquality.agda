{-# OPTIONS --safe --without-K #-}

module Lib.Bool.Instances.DecidableEquality where

open import Lib.Dec

open import Lib.Bool.Type
open import Lib.Bool.Properties

instance
  DecEqBool : DecidableEquality Bool
  DecEqBool = DecProof _≟_
