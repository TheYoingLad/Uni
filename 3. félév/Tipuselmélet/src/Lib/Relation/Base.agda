{-# OPTIONS --safe --without-K #-}
module Lib.Relation.Base where

open import Agda.Primitive
open import Lib.Empty.Base
open import Lib.Sum.Type
open import Agda.Builtin.Equality using (_≡_)

Reflexive : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
Reflexive R = {a : _} → R a a

Irreflexive : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
Irreflexive R = {a : _} → ¬ (R a a)

Symmetric : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
Symmetric R = {a b : _} → R a b → R b a

Transitive : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
Transitive R = {a b c : _} → R a b → R b c → R a c

AntiTransitive : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
AntiTransitive R = {a b c : _} → R a b → R b c → ¬ R a c

AntiSymmetric : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
AntiSymmetric R = {a b : _} → R a b → R b a → a ≡ b

Asymmetric : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
Asymmetric R = {a b : _} → R a b → ¬ (R b a)

StronglyConnected : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
StronglyConnected R = {a b : _} → R a b ⊎ R b a

Universal : ∀{ℓ κ}{A : Set ℓ} → (A → A → Set κ) → Set (ℓ ⊔ κ)
Universal R = ∀ x y → R x y
