{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Instances.IsProp where

open import Lib.Class.IsProp
open import Lib.Fin.Type

instance
  IsPropFin0 : IsProp (Fin 0)
  IsPropFin0 = isProp-proof λ ()
