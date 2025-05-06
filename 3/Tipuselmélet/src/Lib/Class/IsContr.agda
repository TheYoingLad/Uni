{-# OPTIONS --safe --without-K #-}

module Lib.Class.IsContr where

open import Lib.Sigma.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties
open import Lib.Class.IsTrunc

open import Lib.TruncLevel.Literals

IsContr = IsTrunc -2
module IsContr = IsTrunc
isContr = isTrunc {(-2)}

pattern isContr-proof t = isTrunc-proof t

center : ∀{i}{A : Set i} → ⦃ IsContr A ⦄ → A
center = fst isContr

contraction : ∀{i}{A : Set i} → ⦃ H : IsContr A ⦄ → (x : A) → center ≡ x
contraction x = isContrType→≡ isContr {center} {x}

coh-contraction : ∀{i}{A : Set i} → ⦃ H : IsContr A ⦄ → contraction center ≡ refl
coh-contraction = invl≡ (snd isContr (fst isContr))
