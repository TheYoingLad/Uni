{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Properties where

open import Lib.Empty.Type
open import Lib.Dec
open import Lib.Dec.PatternSynonym
open import Lib.Equality

infix 4 _≟_
_≟_ : (a b : ⊥) → Dec (a ≡ b)
_ ≟ _ = yes refl
