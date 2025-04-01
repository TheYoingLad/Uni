{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Instances.Eq where

open import Lib.Class.Eq
open import Lib.Maybe.Type
open import Lib.Unit.Type
open import Lib.Empty.Type

open import Lib.Maybe.Base
open import Lib.Maybe.Properties

open import Lib.Sigma.Type
open import Lib.Equality

instance
  EqMaybe : ∀{i}{A : Set i} → ⦃ Eq A ⦄ → Eq (Maybe A)
  (EqMaybe ⦃ eqA ⦄ Eq.≡ᵗ just x) (just y) with x ≡ᵗ y
  ... | just e , _ , refl = just (cong just e) , ⊤ , refl
  ... | nothing , _ , refl = nothing , ⊥ , refl
  (EqMaybe ⦃ eqA ⦄ Eq.≡ᵗ just x) nothing = nothing , ⊥ , refl
  (EqMaybe ⦃ eqA ⦄ Eq.≡ᵗ nothing) (just x) = nothing , ⊥ , refl
  (EqMaybe ⦃ eqA ⦄ Eq.≡ᵗ nothing) nothing = just refl , ⊤ , refl
  Eq.eqIsJust (EqMaybe ⦃ eqA ⦄) {just x}  {just y}  e with x ≡ᵗ y in eq1
  ... | just eq2 , _ , refl = tt
  ... | nothing  , _ , refl = subst IsJust (cong fst eq1) (eqIsJust ⦃ eqA ⦄ (just-injective e))
  Eq.eqIsJust (EqMaybe ⦃ eqA ⦄) {nothing} {nothing} e = tt
