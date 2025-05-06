{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Base where

open import Lib.Maybe.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Equality.Type

IsJustᵗ : ∀{i}{A : Set i} → Maybe A → Σ Set (λ B → B ≡ ⊤ ⊎ B ≡ ⊥)
IsJustᵗ (just x) = ⊤ , inl refl
IsJustᵗ nothing = ⊥ , inr refl

IsJust : ∀{i}{A : Set i} → Maybe A → Set
IsJust n = fst (IsJustᵗ n)

IsNothingᵗ : ∀{i}{A : Set i} → Maybe A → Σ Set (λ B → B ≡ ⊤ ⊎ B ≡ ⊥)
IsNothingᵗ (just x) = ⊥ , inr refl
IsNothingᵗ nothing = ⊤ , inl refl

IsNothing : ∀{i}{A : Set i} → Maybe A → Set
IsNothing n = fst (IsNothingᵗ n)

elim : ∀{a b}{A : Set a}{B : Maybe A → Set b} →
        ((x : A) → B (just x)) → B nothing → (x : Maybe A) → B x
elim j n (just x) = j x
elim j n nothing  = n

ite : ∀{a b}{A : Set a}{B : Set b} → (A → B) → B → Maybe A → B
ite = elim

fromMaybe : ∀{a}{A : Set a} → A → Maybe A → A
fromMaybe = ite (λ x → x)

ind : ∀{a b}{A : Set a}{B : Maybe A → Set b} →
    ({y : Maybe A}(x : A) → y ≡ just x → B (just x)) →
    ({y : Maybe A} → y ≡ nothing → B nothing) → (x : Maybe A) → B x
ind j n (just x) = j x refl
ind j n nothing = n refl

map : ∀{i j}{A : Set i}{B : Set j} → (A → B) → Maybe A → Maybe B
map _ nothing = nothing
map f (just x) = just (f x)

fromJust : ∀{i}{A : Set i}(m : Maybe A) → .⦃ IsJust m ⦄ → A
fromJust (just a) = a
