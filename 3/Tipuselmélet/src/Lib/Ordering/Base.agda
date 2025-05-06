{-# OPTIONS --safe --without-K #-}

module Lib.Ordering.Base where

open import Lib.Ordering.Type
open import Lib.Equality.Type

case : ∀{i}{A : Set i} → Ordering → A → A → A → A
case LT a b c = a
case EQ a b c = b
case GT a b c = c

ite : ∀{i}{A : Set i} → Ordering → A → A → A → A
ite = case

elim : ∀{i}{A : Ordering → Set i} → (b : Ordering) → A LT → A EQ → A GT → A b
elim LT a b c = a
elim EQ a b c = b
elim GT a b c = c

ind : ∀{i}{A : Ordering → Set i} → (b : Ordering) → (b ≡ LT → A b) → (b ≡ EQ → A b) → (b ≡ GT → A b) → A b
ind LT a b c = a refl
ind EQ a b c = b refl
ind GT a b c = c refl
