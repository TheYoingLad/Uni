{-# OPTIONS --safe --without-K #-}

module Lib.Class.IsSet where

open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties

open import Lib.Nat.Type

open import Lib.Sigma.Type

open import Lib.Class.IsTrunc
open import Lib.Class.IsProp

open import Lib.TruncLevel.Literals

IsSet = IsTrunc 0
module IsSet = IsTrunc
isSet = isTrunc {0}

pattern isSet-proof t = isTrunc-proof t

UIP = IsSet
module UIP = IsSet
uip = isSet

isSet' : ∀{i}{A : Set i}⦃ H : IsSet A ⦄(x y : A)(p q : x ≡ y) → p ≡ q
isSet' ⦃ isSet-proof isSet ⦄ x y p q = fst (isSet x y p q)

isSet'→IsSet : ∀{i}{A : Set i} → ((x y : A)(p q : x ≡ y) → p ≡ q) → IsSet A
isSet'→IsSet f = isSet-proof λ x y → isProp ⦃ isProp'→IsProp {A = x ≡ y} (f x y) ⦄

{-
-- For historical reasons:
isSet'→IsSet : ∀{i}{A : Set i} → ((x y : A)(p q : x ≡ y) → p ≡ q) → IsSet A
isSet'→IsSet f = isSet-proof (λ {x .x refl q → (sym (invl≡ q) ◾ sym (f x x refl (sym q ◾ q)) ◾ f x x refl q) , λ {refl → cong (refl ◾_) (invl≡ (f x x refl refl))}})
-}

K : ∀{i j}{A : Set i}⦃ H : IsSet A ⦄(x : A)(P : x ≡ x → Set j) → P refl → (q : x ≡ x) → P q
K ⦃ H ⦄ x P e q = subst P (isSet' ⦃ H ⦄ x x refl q) e

K→isSet' : ∀{i}{A : Set i}(K : (x : A)(P : x ≡ x → Set i) → P refl → (q : x ≡ x) → P q) → (x y : A)(p q : x ≡ y) → p ≡ q
K→isSet' K x y p q = K x (λ e → p ≡ (e ◾ p)) (sym (idl≡ p)) (q ◾ sym p) ◾ assoc-trans q (sym p) p ◾ cong (q ◾_) (invl≡ p)

K→IsSet : ∀{i}{A : Set i}(K : (x : A)(P : x ≡ x → Set i) → P refl → (q : x ≡ x) → P q) → IsSet A
K→IsSet K = isSet'→IsSet (K→isSet' K)

instance
  IsProp→IsSet : ∀{i}{A : Set i} → ⦃ IsProp A ⦄ → IsSet A
  IsProp→IsSet ⦃ H ⦄ = SucIsTrunc ⦃ H ⦄
