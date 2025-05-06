{-# OPTIONS --safe --without-K #-}

module Lib.Bool.Instances.Ord where

open import Lib.Class.Ord

open import Lib.Bool.Type
open import Lib.Bool.Instances.Eq

open import Lib.Ordering.Type

-----------------------------------------

open import Lib.Sigma.Type
open import Lib.Equality.Type
open import Lib.Equality.Base

instance
  OrdBool : Ord Bool
  Ord.eq OrdBool = EqBool
  Ord.compare OrdBool false false = EQ
  Ord.compare OrdBool false true = LT
  Ord.compare OrdBool true false = GT
  Ord.compare OrdBool true true = EQ
  Ord.flippable OrdBool {false} {false} = (λ ()) , (λ ())
  Ord.flippable OrdBool {false} {true} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable OrdBool {true} {false} = sym , sym
  Ord.flippable OrdBool {true} {true} = (λ ()) , (λ ())
  Ord.equality OrdBool {false} = refl
  Ord.equality OrdBool {true} = refl
  Ord.consistencyWithEq OrdBool {false} {false} _ = refl
  Ord.consistencyWithEq OrdBool {false} {true} ()
  Ord.consistencyWithEq OrdBool {true} {false} ()
  Ord.consistencyWithEq OrdBool {true} {true} _ = refl
