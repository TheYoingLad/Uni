{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Instances.Eq where

open import Lib.Sigma.Type
open import Lib.Sigma.Properties
open import Lib.Class.Eq
open import Lib.Equality

open import Lib.Empty.Type
open import Lib.Unit.Type

open import Lib.Maybe.Type
open import Lib.Maybe.Base

instance
  Eq× : ∀{i j}{A : Set i}{B : Set j} → ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → Eq (A × B)
  (Eq× ⦃ eqA ⦄ ⦃ eqB ⦄ Eq.≡ᵗ (a1 , b1)) (a2 , b2) with a1 ≡ᵗ a2
  ... | nothing , _ , refl = nothing , ⊥ , refl
  ... | just x , _ , refl with b1 ≡ᵗ b2
  ... | just e , _ , refl = just (x ,= trans (substconst x) e) , ⊤ , refl
  ... | nothing , _ , refl = nothing , ⊥ , refl
  Eq.eqIsJust (Eq× ⦃ eqA ⦄ ⦃ eqB ⦄) {a1 , b1} {a2 , b2} e with a1 ≡ᵗ a2 in eq1
  ... | nothing , _ , refl = subst IsJust (cong fst eq1) (eqIsJust ⦃ eqA ⦄ (cong fst e))
  ... | just eq2 , _ , refl with b1 ≡ᵗ b2 in eq3
  ... | just eq4 , _ , refl = tt
  ... | nothing , _ , refl = subst IsJust (cong fst eq3) (eqIsJust ⦃ eqB ⦄ (cong snd e))

  -- For EqΣ UIP is needed!
