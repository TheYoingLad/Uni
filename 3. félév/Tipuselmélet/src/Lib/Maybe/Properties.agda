{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Properties where

open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym

just-injective : ∀{i}{A : Set i}{x y : A} → just x ≡ just y → x ≡ y
just-injective {x = x} = cong (fromMaybe x)

just-nothing-disjunctive : ∀{i}{A : Set i}{x : A} → just x ≢ nothing
just-nothing-disjunctive ()

≡-dec-Maybe : ∀{i}{A : Set i} → ((a b : A) → Dec (a ≡ b)) → (x y : Maybe A) → Dec (x ≡ y)
≡-dec-Maybe p (just x) (just y) with p x y
... | yes refl = yes refl
... | no ¬a = no λ e → ¬a (just-injective e)
≡-dec-Maybe _ (just x) nothing = no λ ()
≡-dec-Maybe _ nothing (just y) = no λ ()
≡-dec-Maybe _ nothing nothing  = yes refl
