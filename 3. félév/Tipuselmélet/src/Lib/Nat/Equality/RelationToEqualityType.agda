{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Equality.RelationToEqualityType where

open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.Nat.Properties
open import Lib.Nat.Equality.Type
open import Lib.Nat.Equality.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Unit.Type
open import Lib.Sigma.Type

≡→≡ℕ : ∀{a b} → .(a ≡ b) → a ≡ℕ b
≡→≡ℕ {zero} {zero} _ = reflℕ 0
≡→≡ℕ {suc a} {suc b} e = ≡→≡ℕ {a} {b} (suc-injective e)

≡ℕ→≡ : ∀{a b} → .(a ≡ℕ b) → a ≡ b
≡ℕ→≡ {zero} {zero} _ = refl
≡ℕ→≡ {zero} {suc b} ()
≡ℕ→≡ {suc a} {zero} ()
≡ℕ→≡ {suc a} {suc b} e = cong suc (≡ℕ→≡ {a} {b} e)

≢→≢ℕ : ∀{a b} → .(a ≢ b) → a ≢ℕ b
≢→≢ℕ {zero} {zero} f with .(f refl)
... | ()
≢→≢ℕ {zero} {suc b} f = tt
≢→≢ℕ {suc a} {zero} f = tt
≢→≢ℕ {suc a} {suc b} f = ≢→≢ℕ {a} {b} (λ e → f (cong suc e))

≢ℕ→≢ : ∀{a b} → .(a ≢ℕ b) → a ≢ b
≢ℕ→≢ {zero} {suc b} ne ()
≢ℕ→≢ {suc a} {suc b} ne e = ≢ℕ→≢ {a} {b} ne (suc-injective e)

instance
  IsZero→≡ : {n : ℕ} → .⦃ IsZero n ⦄ → n ≡ 0
  IsZero→≡ {zero} = refl

  IsNotZero→≡ : {n : ℕ} → .⦃ IsNotZero n ⦄ → Σ ℕ λ k → n ≡ suc k
  IsNotZero→≡ {suc n} = n , refl
