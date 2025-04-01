{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.Stream.Properties where

open import Lib.Containers.Stream.Type
open import Lib.Containers.Stream.Base
open import Lib.Containers.Stream.Bisimilarity
open import Lib.Equality

reflS : ∀{i}{A : Set i}(xs : Stream A) → xs ≈S xs
head-≡ (reflS xs) = refl
tail-≈ (reflS xs) = reflS (tail xs)

symS : ∀{i}{A : Set i}{xs ys : Stream A} → xs ≈S ys → ys ≈S xs
head-≡ (symS e) = sym (head-≡ e)
tail-≈ (symS e) = symS (tail-≈ e)

transS : ∀{i}{A : Set i}{xs ys zs : Stream A} → xs ≈S ys → ys ≈S zs → xs ≈S zs
head-≡ (transS e1 e2) = trans (head-≡ e1) (head-≡ e2)
tail-≈ (transS e1 e2) = transS (tail-≈ e1) (tail-≈ e2)

≡→≈ : ∀{i}{A : Set i}{xs ys : Stream A} → xs ≡ ys → xs ≈S ys
≡→≈ {xs = xs} refl = reflS xs
