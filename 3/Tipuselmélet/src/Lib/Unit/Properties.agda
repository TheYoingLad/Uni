{-# OPTIONS --safe --without-K #-}

module Lib.Unit.Properties where

open import Lib.Unit.Type
open import Lib.Dec
open import Lib.Dec.PatternSynonym
open import Lib.Equality.Type

⊤η : (a b : ⊤) → a ≡ b
⊤η a b = refl

infix 4 _≟_
_≟_ : (a b : ⊤) → Dec (a ≡ b)
_ ≟ _ = yes refl
