{-# OPTIONS --safe --without-K #-}

module Lib.WorkInProgressConcept.DoNotIncludeInLib.Problem (ℓ : _) (A : Set ℓ) where

open import Lib.Relation
open import Lib.Containers.Stream.Type
open import Lib.Containers.Stream.Properties
open import Lib.Containers.Stream.Bisimilarity
open import Lib.Relation.Notation {ℓ} {Stream A} _≈S_ transS (λ {xs} → reflS xs)

r : ∀{i}{A : Set i}(xs ys zs : Stream A) → xs ≈S ys → ys ≈S zs → xs ≈S zs
r xs ys zs e1 e2 = {!_≡⟨_⟩_!}
