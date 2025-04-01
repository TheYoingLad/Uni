{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Equality.Type where

open import Lib.Nat.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
open import Lib.Equality.Type
open import Lib.Unit.Type
open import Lib.Empty.Type

infix 4 _≤ℕᵗ_ _<ℕᵗ_ _>ℕᵗ_ _≥ℕᵗ_ _≡ℕᵗ_ _≢ℕᵗ_ _≤ℕ_ _<ℕ_ _>ℕ_ _≥ℕ_ _≡ℕ_ _≢ℕ_

_≡ℕᵗ_ : ℕ → ℕ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
zero ≡ℕᵗ zero = ⊤ , inl refl
zero ≡ℕᵗ suc y = ⊥ , inr refl
suc x ≡ℕᵗ zero = ⊥ , inr refl
suc x ≡ℕᵗ suc y = x ≡ℕᵗ y

_≡ℕ_ : ℕ → ℕ → Set
x ≡ℕ y = fst (x ≡ℕᵗ y)

_≢ℕᵗ_ : ℕ → ℕ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
zero ≢ℕᵗ zero = ⊥ , inr refl
zero ≢ℕᵗ suc y = ⊤ , inl refl
suc x ≢ℕᵗ zero = ⊤ , inl refl
suc x ≢ℕᵗ suc y = x ≢ℕᵗ y

_≢ℕ_ : ℕ → ℕ → Set
x ≢ℕ y = fst (x ≢ℕᵗ y)

_≤ℕᵗ_ : ℕ → ℕ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
zero ≤ℕᵗ _ = ⊤ , inl refl
suc x ≤ℕᵗ zero = ⊥ , inr refl
suc x ≤ℕᵗ suc y = x ≤ℕᵗ y

_≤ℕ_ : ℕ → ℕ → Set
x ≤ℕ y = fst (x ≤ℕᵗ y)

_<ℕᵗ_ : ℕ → ℕ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
_ <ℕᵗ zero = ⊥ , inr refl
zero <ℕᵗ suc y = ⊤ , inl refl
suc x <ℕᵗ suc y = x <ℕᵗ y

_<ℕ_ : ℕ → ℕ → Set
x <ℕ y = fst (x <ℕᵗ y)

_>ℕᵗ_ : ℕ → ℕ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
zero >ℕᵗ _ = ⊥ , inr refl
suc x >ℕᵗ zero = ⊤ , inl refl
suc x >ℕᵗ suc y = x >ℕᵗ y

_>ℕ_ : ℕ → ℕ → Set
x >ℕ y = fst (x >ℕᵗ y)

_≥ℕᵗ_ : ℕ → ℕ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
_ ≥ℕᵗ zero = ⊤ , inl refl
zero ≥ℕᵗ suc y = ⊥ , inr refl
suc x ≥ℕᵗ suc y = x ≥ℕᵗ y

_≥ℕ_ : ℕ → ℕ → Set
x ≥ℕ y = fst (x ≥ℕᵗ y)
