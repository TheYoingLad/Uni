{-# OPTIONS --safe --without-K #-}

module Lib.Product.Instances.Eq where

open import Lib.Product.Type
open import Lib.Product.Properties
open import Lib.Class.Eq
import Lib.Sigma.Type as Σ
open import Lib.Equality

open import Lib.Empty.Type
open import Lib.Unit.Type

open import Lib.Maybe.Type
open import Lib.Maybe.Base

instance
  Eq× : ∀{i j}{A : Set i}{B : Set j} → ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → Eq (A × B)
  (Eq× ⦃ eqA ⦄ ⦃ eqB ⦄ Eq.≡ᵗ (a1 , b1)) (a2 , b2) with a1 ≡ᵗ a2
  ... | nothing Σ., _ Σ., refl = nothing Σ., ⊥ Σ., refl
  ... | just x Σ., _ Σ., refl with b1 ≡ᵗ b2
  ... | just e Σ., _ Σ., refl = just (snd ×≡ (x , e)) Σ., ⊤ Σ., refl
  ... | nothing Σ., _ Σ., refl = nothing Σ., ⊥ Σ., refl
  Eq.eqIsJust (Eq× ⦃ eqA ⦄ ⦃ eqB ⦄) {a1 , b1} {a2 , b2} e with a1 ≡ᵗ a2 in eq1
  ... | nothing Σ., _ Σ., refl = subst IsJust (cong Σ.fst eq1) (eqIsJust ⦃ eqA ⦄ (fst (fst ×≡ e)))
  ... | just eq2 Σ., _ Σ., refl with b1 ≡ᵗ b2 in eq3
  ... | just eq4 Σ., _ Σ., refl = tt
  ... | nothing Σ., _ Σ., refl = subst IsJust (cong Σ.fst eq3) (eqIsJust ⦃ eqB ⦄ (snd (fst ×≡ e)))
