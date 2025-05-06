{-# OPTIONS --safe --without-K #-}

module Lib.IndCoFin.Properties where

open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.CoNat.Literals
open import Lib.IndCoFin.Type
open import Lib.IndCoFin.Base
open import Lib.Unit.Type
open import Lib.Maybe.Type
open import Lib.Equality.Type

CofinIsNeverZero : {n : ℕ∞} → IndCoFin n → IsNotZero∞ n
CofinIsNeverZero {n} izero with pred∞ n
... | just _ = tt
CofinIsNeverZero {n} (isuc x) with pred∞ n
... | just _ = tt

noICF0 : ∀{ℓ}{A : Set ℓ} → IndCoFin 0 → A
noICF0 i with CofinIsNeverZero i
... | ()

prop1ICF : (i j : IndCoFin 1) → i ≡ j
prop1ICF izero izero = refl
prop1ICF izero (isuc j) = noICF0 j
prop1ICF (isuc i) j = noICF0 i
