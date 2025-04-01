{-# OPTIONS --safe --without-K #-}

module Lib.Isomorphism.Properties where

open import Lib.Isomorphism.Type
open import Lib.Equality.Type
open import Lib.Equality.Base

refl≅ : ∀{i}{A : Set i} → A ≅ A
refl≅ = ≅-proof (λ x → x) (λ x → x) (λ _ → refl) (λ _ → refl)

sym≅ : ∀{i j}{A : Set i}{B : Set j} → A ≅ B → B ≅ A
sym≅ i = ≅-proof (coe← i) (coe→ i) (id→← i) (id←→ i)

trans≅ : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → A ≅ B → B ≅ C → A ≅ C
trans≅ i1 i2 = ≅-proof
               (λ x → coe→ i2 (coe→ i1 x))
               (λ x → coe← i1 (coe← i2 x))
               (λ a → trans (cong (coe← i1) (id←→ i2 (coe→ i1 a))) (id←→ i1 a))
               (λ b → trans (cong (coe→ i2) (id→← i1 (coe← i2 b))) (id→← i2 b))

≡→≅ : ∀{i}{A B : Set i} → A ≡ B → A ≅ B
≡→≅ refl = ≅-proof (λ z → z) (λ z → z) (λ _ → refl) (λ _ → refl)

coe←-injective : ∀{i j}{A : Set i}{B : Set j} → (iso : A ≅ B) → {b1 b2 : B} → coe← iso b1 ≡ coe← iso b2 → b1 ≡ b2
coe←-injective iso {b1} {b2} e = sym (id→← iso b1) ◾ cong (coe→ iso) e ◾ id→← iso b2

coe→-injective : ∀{i j}{A : Set i}{B : Set j} → (iso : A ≅ B) → {a1 a2 : A} → coe→ iso a1 ≡ coe→ iso a2 → a1 ≡ a2
coe→-injective iso {a1} {a2} e = sym (id←→ iso a1) ◾ cong (coe← iso) e ◾ id←→ iso a2
