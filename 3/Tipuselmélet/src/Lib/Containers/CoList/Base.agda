{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoList.Base where

open import Lib.Containers.CoList.Type
open import Lib.Containers.CoList.PatternSynonym
open import Lib.Containers.List.Type using (List)
import Lib.Containers.List.Type as L
open import Lib.Sum.Type
open import Lib.Sigma.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Nat.Type
open import Lib.CoNat.Type
open import Lib.CoNat.PatternSynonym
open import Lib.Level
open import Lib.Maybe.Type
open import Lib.Equality.Type

Nullᵗ : ∀{i}{A : Set i} → CoList A → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
Nullᵗ xs with uncons xs
... | _ ∷∞ _ = ⊥ , inr refl
... | []∞ = ⊤ , inl refl

Null : ∀{i}{A : Set i} → CoList A → Set
Null xs = fst (Nullᵗ xs)

NotNullᵗ : ∀{i}{A : Set i} → CoList A → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
NotNullᵗ xs with uncons xs
... | []∞ = ⊥ , inr refl
... | _ ∷∞ _ = ⊤ , inl refl

NotNull : ∀{i}{A : Set i} → CoList A → Set
NotNull xs = fst (NotNullᵗ xs)

replicate : ∀{i}{A : Set i} → ℕ → A → CoList A
replicate zero a = []
replicate (suc n) a = a ∷ replicate n a

coreplicate : ∀{i}{A : Set i} → ℕ∞ → A → CoList A
uncons (coreplicate n a) with pred∞ n
uncons (coreplicate n a) | zero∞ = []∞
uncons (coreplicate n a) | suc∞ n' = a ∷∞ coreplicate n' a

repeat : ∀{i}{A : Set i} → A → CoList A
repeat = coreplicate ∞

take : ∀{i}{A : Set i} → ℕ → CoList A → List A
take zero xs = L.[]
take (suc n) xs with uncons xs
take (suc n) xs | []∞ = L.[]
take (suc n) xs | y ∷∞ ys = y L.∷ take n ys

cotake : ∀{i}{A : Set i} → ℕ∞ → CoList A → CoList A
uncons (cotake n xs) with pred∞ n
uncons (cotake n xs) | zero∞   = []∞
uncons (cotake n xs) | suc∞ n' with uncons xs
uncons (cotake n xs) | suc∞ n' | []∞     = []∞
uncons (cotake n xs) | suc∞ n' | y ∷∞ ys = y ∷∞ cotake n' ys

length : ∀{i}{A : Set i} → CoList A → ℕ∞
pred∞ (length xs) with uncons xs
... | []∞ = zero∞
... | _ ∷∞ ys = suc∞ (length ys)

Unwrap-uncons : ∀{a}{A : Set a} → Maybe (A × CoList A) → Set a
Unwrap-uncons {a} {A} nothing = Lift a ⊤
Unwrap-uncons {A = A} (just _) = A × CoList A

unwrap-uncons : ∀{a}{A : Set a} → (l : Maybe (A × CoList A)) → Unwrap-uncons l
unwrap-uncons nothing = _
unwrap-uncons (just x) = x
