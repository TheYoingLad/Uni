{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Type where

open import Agda.Primitive using (_⊔_)
open import Agda.Builtin.Sigma public

infix 2 Σ-syntax

Σ-syntax : ∀{a b}(A : Set a) → (A → Set b) → Set (a ⊔ b)
Σ-syntax = Σ

syntax Σ-syntax A (λ x → B) = [ x ∶ A ] × B

∃ : ∀ {a b}{A : Set a} → (A → Set b) → Set (a ⊔ b)
∃ = Σ _

infix 2 ∃-syntax

∃-syntax : ∀ {a b}{A : Set a} → (A → Set b) → Set (a ⊔ b)
∃-syntax = ∃

syntax ∃-syntax (λ x → B) = ∃[ x ] B

infix 4 -,_
-,_ : ∀{a b}{A : Set a} {B : A → Set b} {x} → B x → Σ _ B
-, y = _ , y

infixr 2 _×_
_×_ : ∀{i j}(A : Set i)(B : Set j) → Set (i ⊔ j)
A × B = Σ A (λ _ → B)

infixr 4 _,'_
_,'_ : ∀{a b}{A : Set a}{B : Set b} → A → B → A × B
_,'_ = _,_

infix 0 _↔_
_↔_ : ∀{i j} → Set i → Set j → Set (i ⊔ j)
A ↔ B = (A → B) × (B → A)

ιΣ : ∀{a b}(A : Set a) → (⦃ A ⦄ → Set b) → Set (a ⊔ b)
ιΣ A B = Σ A (λ a → B ⦃ a ⦄)

infix 4 -ι,_
-ι,_ : ∀{a b}{A : Set a} {B : ⦃ A ⦄ → Set b} ⦃ x ⦄ → B ⦃ x ⦄ → ιΣ A (λ ⦃ a ⦄ → B ⦃ a ⦄)
-ι, y = _ , y
