{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Base where

open import Lib.Level
open import Lib.Empty.Type
open import Lib.Equality.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
open import Lib.Sum.Base
open import Lib.Unit.Type

exfalso : ∀ {w} {Whatever : Set w} → ⊥ → Whatever
exfalso ()

exfalso-irrelevant : ∀ {w} {Whatever : Set w} → .⊥ → Whatever
exfalso-irrelevant ()

infixl 30 ¬_ ¬ι_
¬_ : ∀{i} → Set i → Set i
¬ A = A → ⊥

¬ι_ : ∀{i} → Set i → Set i
¬ι A = ⦃ a : A ⦄ → ⊥

infixl 30 ¬ᵗ_
¬ᵗ_ : (t : Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)) →
      Σ {_} {level 1} Set (λ A → case (snd t) (λ {refl → A ≡ ⊥}) (λ {refl → A ≡ ⊤}))
¬ᵗ (.⊤ , inl refl) = ⊥ , refl
¬ᵗ (.⊥ , inr refl) = ⊤ , refl

contradiction : ∀{i j}{P : Set i}{Whatever : Set j} → P → ¬ P → Whatever
contradiction p ¬p = exfalso (¬p p)

contraposition : ∀{i j}{P : Set i}{Q : Set j} → (P → Q) → ¬ Q → ¬ P
contraposition f ¬q p = contradiction (f p) ¬q

weaken : {X : Set} → X → ¬ ¬ X
weaken x = λ ¬x → ¬x x
