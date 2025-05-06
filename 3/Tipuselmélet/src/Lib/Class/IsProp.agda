{-# OPTIONS --safe --without-K #-}

module Lib.Class.IsProp where

open import Lib.Class.IsTrunc
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties
open import Lib.Sigma.Type

open import Lib.TruncLevel.Literals

IsProp = IsTrunc -1
module IsProp = IsTrunc
isProp = isTrunc {(-1)}

pattern isProp-proof t = isTrunc-proof t

isProp' : ∀{i}{A : Set i} → ⦃ IsProp A ⦄ → (x y : A) → x ≡ y
isProp' ⦃ isProp-proof isProp ⦄ x y = fst (isProp x y)

IsProp→K : ∀{i j}{A : Set i} → ⦃ IsProp A ⦄ → (x : A)(P : x ≡ x → Set j) → P refl → (q : x ≡ x) → P q
IsProp→K ⦃ isProp-proof isProp ⦄ x P p-refl q = subst P (sym (snd (isProp x x) refl) ◾ snd (isProp x x) q) p-refl

isProp'→IsProp : ∀{i}{A : Set i} → ((x y : A) → x ≡ y) → IsProp A
isProp'→IsProp f = isProp-proof λ x y → ((sym (f x x) ◾ f x y) , λ {refl → invl≡ (f x x)})
