{-# OPTIONS --safe --without-K #-}

module Lib.Bool.Instances.IsSet where

open import Lib.Bool.Type
open import Lib.Bool.Properties
open import Lib.Class.IsSet

open import Lib.Dec.Type
open import Lib.Dec.InstanceGenerators.IsSet

instance
  isSetBool : IsSet Bool
  isSetBool = DecidableEquality→IsSet (DecProof _≟_)
