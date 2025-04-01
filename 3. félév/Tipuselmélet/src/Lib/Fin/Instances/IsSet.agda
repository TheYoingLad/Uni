{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Instances.IsSet where

open import Lib.Class.IsSet
open import Lib.Dec.InstanceGenerators.IsSet
open import Lib.Fin.Type
open import Lib.Fin.Instances.DecidableEquality

instance
  IsSetFin : ∀{n} → IsSet (Fin n)
  IsSetFin = DecidableEquality→IsSet DecEqFin
