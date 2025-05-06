{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Instances.Ord where

open import Lib.Class.Eq
open import Lib.Class.Ord
open import Lib.Sum.Type
open import Lib.Sum.Instances.Eq
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Empty.Base
open import Lib.Ordering.Type

instance
  Ord⊎ : ∀{i j}{A : Set i}{B : Set j} → ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → Ord (A ⊎ B)
  Ord.eq Ord⊎ = Eq⊎
  
  Ord.compare Ord⊎ (inl a) (inl a₁) = compare a a₁
  Ord.compare Ord⊎ (inl a) (inr b)  = LT
  Ord.compare Ord⊎ (inr b) (inl a)  = GT
  Ord.compare Ord⊎ (inr b) (inr b₁) = compare b b₁
  
  Ord.flippable (Ord⊎ ⦃ ordA ⦄) {inl a} {inl a₁} with compare a a₁ in eq1
  ... | LT = (λ _ → fst (flippable ⦃ ordA ⦄) eq1) , (λ _ → refl)
  ... | EQ = (λ ()) , λ e → trans (sym eq1) (snd (flippable ⦃ ordA ⦄) e)
  ... | GT = (λ ()) , λ e → trans (sym eq1) (snd (flippable ⦃ ordA ⦄) e)
  Ord.flippable Ord⊎ {inl a} {inr b} = (λ _ → refl) , (λ _ → refl)
  Ord.flippable Ord⊎ {inr b} {inl a} = (λ ()) , (λ ())
  Ord.flippable (Ord⊎ ⦃ _ ⦄ ⦃ ordB ⦄) {inr b} {inr b₁} with compare b b₁ in eq1
  ... | LT = (λ _ → fst (flippable ⦃ ordB ⦄) eq1) , λ _ → refl
  ... | EQ = (λ ()) , λ e → trans (sym eq1) (snd (flippable ⦃ ordB ⦄) e)
  ... | GT = (λ ()) , λ e → trans (sym eq1) (snd (flippable ⦃ ordB ⦄) e)
  
  Ord.equality (Ord⊎ ⦃ ordA ⦄ ⦃ ordB ⦄) {inl a} = equality ⦃ ordA ⦄ {a}
  Ord.equality (Ord⊎ ⦃ ordA ⦄ ⦃ ordB ⦄) {inr b} = equality ⦃ ordB ⦄ {b}

  Ord.consistencyWithEq (Ord⊎ ⦃ ordA ⦄ ⦃ ordB ⦄) {inl a} {inl a₁} e with a ≡ᵗ a₁ in eq1
  ... | just _ , _ , refl = consistencyWithEq ⦃ ordA ⦄ {a} {a₁} (cast (sym (cong (λ t → fst (snd t)) eq1)) tt)
  ... | nothing , _ , refl = exfalso e
  Ord.consistencyWithEq (Ord⊎ ⦃ ordA ⦄ ⦃ ordB ⦄) {inr b} {inr b₁} e with b ≡ᵗ b₁ in eq1
  ... | just _ , _ , refl = consistencyWithEq ⦃ ordB ⦄ {b} {b₁} (cast (sym (cong (λ t → fst (snd t)) eq1)) tt)
  ... | nothing , _ , refl = exfalso e
