{-# OPTIONS --safe --without-K #-}

module Lib.Dec.InstanceGenerators.IsSet where

open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym

open import Lib.Class.IsSet

open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties

open import Lib.Empty.Type
open import Lib.Empty.Base

open import Lib.Sigma.Type

open import Lib.Level

Hedberg-lemma-matcher : ∀{i}{A : Set i} → DecidableEquality A → (x y : A) → (p : x ≡ y) → Set i
Hedberg-lemma-matcher {i} (DecProof decide) x y p with decide x x | decide x y
... | yes e | yes q = (sym e ◾ q) ≡ p
... | yes a | no b = Lift i ⊥
... | no b | yes a = Lift i ⊥
... | no b | no b₁ = Lift i ⊥

Hedberg-lemma : ∀{i}{A : Set i} → (decEq : DecidableEquality A) → (x y : A) → (p : x ≡ y) → Hedberg-lemma-matcher decEq x y p
Hedberg-lemma (DecProof decide) x .x refl with decide x x
... | yes a = invl≡ a
... | no b = lift (b refl)

Hedberg : ∀{i}{A : Set i} → DecidableEquality A → (a₁ a₂ : A)(p₁ p₂ : a₁ ≡ a₂) → p₁ ≡ p₂
Hedberg decEq@(DecProof decide) a₁ a₂ p₁ p₂ with Hedberg-lemma decEq a₁ a₂ p₁ | Hedberg-lemma decEq a₁ a₂ p₂
... | p₁-dec | p₂-dec with decide a₁ a₁ | decide a₁ a₂
... | yes eq | yes p₃ = sym p₁-dec ◾ p₂-dec
... | yes _ | no b = contradiction p₁ b
... | no b | yes a = contradiction refl b
... | no a | no b = contradiction refl a

DecidableEquality→IsSet : ∀{i}{A : Set i} → DecidableEquality A → IsSet A
DecidableEquality→IsSet decEq = isSet'→IsSet (Hedberg decEq)
