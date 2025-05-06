{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoList.Bisimilarity where

open import Lib.Containers.CoList.Type
open import Lib.Containers.CoList.PatternSynonym
open import Lib.Equality.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Maybe.Type
open import Lib.Sigma.Type
open import Lib.Level

infix 4 _≈L_ _≈L′_
record _≈L_ {a} {A : Set a} (xs ys : CoList A) : Set a

data _≈L′_ {a}{A : Set a} : Maybe (A × CoList A) → Maybe (A × CoList A) → Set a where
  nothing-refl   : nothing ≈L′ nothing
  cong-just-cons : ∀{x xs y ys} → x ≡ y → xs ≈L ys → x ∷∞ xs ≈L′ y ∷∞ ys

record _≈L_ {a} {A} xs ys where
  coinductive
  field prove : uncons xs ≈L′ uncons ys

open _≈L_ public
