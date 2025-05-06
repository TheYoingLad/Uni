{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Instances.Eq where

open import Lib.Class.Eq
open import Lib.Sum.Type
open import Lib.Sum.Properties
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Unit.Type
open import Lib.Empty.Type

instance
  Eq⊎ : ∀{i j}{A : Set i}{B : Set j} → ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → Eq (A ⊎ B)
  (Eq⊎ ⦃ i1 ⦄ ⦃ i2 ⦄ Eq.≡ᵗ inl a) (inl a₁) with a ≡ᵗ a₁
  ... | just e , _ , refl = just (cong inl e) , ⊤ , refl
  ... | nothing , _ , refl = nothing , ⊥ , refl
  (Eq⊎ ⦃ i1 ⦄ ⦃ i2 ⦄ Eq.≡ᵗ inl a) (inr b) = nothing , ⊥ , refl
  (Eq⊎ ⦃ i1 ⦄ ⦃ i2 ⦄ Eq.≡ᵗ inr b) (inl a) = nothing , ⊥ , refl
  (Eq⊎ ⦃ i1 ⦄ ⦃ i2 ⦄ Eq.≡ᵗ inr b) (inr b₁) with b ≡ᵗ b₁
  ... | just e , _ , refl = just (cong inr e) , ⊤ , refl
  ... | nothing , _ , refl = nothing , ⊥ , refl
  Eq.eqIsJust (Eq⊎ ⦃ i1 ⦄     ) {inl a} {inl a₁} e with a ≡ᵗ a₁ in eq1
  ... | just _ , _ , refl = tt
  ... | nothing , _ , refl = subst IsJust (cong fst eq1) (eqIsJust ⦃ i1 ⦄ {a} {a₁} (inl-injective e))
  Eq.eqIsJust (Eq⊎ ⦃ _ ⦄ ⦃ i2 ⦄) {inr b} {inr b₁} e with b ≡ᵗ b₁ in eq1
  ... | just _ , _ , refl = tt
  ... | nothing , _ , refl = subst IsJust (cong fst eq1) (eqIsJust ⦃ i2 ⦄ {b} {b₁} (inr-injective e))
