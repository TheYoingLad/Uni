{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Instances.IsContr where

open import Lib.Class.IsContr
open import Lib.Fin.Type
open import Lib.Sigma.Type
open import Lib.Equality.Type

instance
  IsContrFin1 : IsContr (Fin 1)
  IsContrFin1 = isContr-proof (fzero , (λ {fzero → refl}))
