{-# OPTIONS --safe --without-K #-}

module Lib.Dec.Base where

open import Lib.Dec.Type
open import Lib.Empty.Type
open import Lib.Unit.Type
open import Lib.Sum.Type

True : ∀{i}{A : Set i} → Dec A → Set
True (inl _) = ⊤
True (inr _) = ⊥

False : ∀{i}{A : Set i} → Dec A → Set
False (inl _) = ⊥
False (inr _) = ⊤

extractTrue : ∀{i}{A : Set i}(da : Dec A) → .⦃ True da ⦄ → A
extractTrue (inl a) = a

extractFalse : ∀{i}{A : Set i}(da : Dec A) → .⦃ False da ⦄ → A → ⊥
extractFalse (inr a) = a
