{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Equality.Bool where

open import Lib.Nat.Type
open import Lib.Nat.Equality.Type
open import Lib.Nat.Equality.Base
open import Lib.Nat.Equality.RelationToEqualityType
open import Lib.Class.Eq
open import Lib.Class.Ord
open import Lib.Nat.Instances.Eq
open import Lib.Nat.Instances.Ord

open import Lib.Bool.Base

open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Unit.Type
open import Lib.Equality.Type
open import Lib.Equality.Base

T>=→≥ℕ : (x y : ℕ) → T (x >= y) → x ≥ℕ y
T>=→≥ℕ zero zero e = tt
T>=→≥ℕ (suc x) zero e = tt
T>=→≥ℕ (suc x) (suc y) e = T>=→≥ℕ x y e

≥ℕ→T>= : (x y : ℕ) → x ≥ℕ y → T (x >= y)
≥ℕ→T>= zero zero e = tt
≥ℕ→T>= (suc x) zero e = tt
≥ℕ→T>= (suc x) (suc y) e = ≥ℕ→T>= x y e

T>→>ℕ : (x y : ℕ) → T (x > y) → x >ℕ y
T>→>ℕ (suc x) zero e = tt
T>→>ℕ (suc x) (suc y) e = T>→>ℕ x y e

>ℕ→T> : (x y : ℕ) → x >ℕ y → T (x > y)
>ℕ→T> (suc x) zero e = tt
>ℕ→T> (suc x) (suc y) e = >ℕ→T> x y e

T<=→≤ℕ : (x y : ℕ) → T (x <= y) → x ≤ℕ y
T<=→≤ℕ zero zero e = tt
T<=→≤ℕ zero (suc y) e = tt
T<=→≤ℕ (suc x) (suc y) e = T<=→≤ℕ x y e

≤ℕ→T<= : (x y : ℕ) → x ≤ℕ y → T (x <= y)
≤ℕ→T<= zero zero e = tt
≤ℕ→T<= zero (suc y) e = tt
≤ℕ→T<= (suc x) (suc y) e = ≤ℕ→T<= x y e

T<→<ℕ : (x y : ℕ) → T (x < y) → x <ℕ y
T<→<ℕ zero (suc y) e = tt
T<→<ℕ (suc x) (suc y) e = T<→<ℕ x y e

<ℕ→T< : (x y : ℕ) → x <ℕ y → T (x < y)
<ℕ→T< zero (suc y) e = tt
<ℕ→T< (suc x) (suc y) e = <ℕ→T< x y e

T==→≡ℕ : (x y : ℕ) → T (x == y) → x ≡ℕ y
T==→≡ℕ zero zero e = tt
T==→≡ℕ (suc x) (suc y) e with x ≡ᵗ y in eq1
... | just p , t , r = ≡→≡ℕ p

≡ℕ→T== : (x y : ℕ) → x ≡ℕ y → T (x == y)
≡ℕ→T== zero zero e = tt
≡ℕ→T== (suc x) (suc y) e with x ≡ᵗ y in eq1
... | just p , t , r = tt
... | nothing , t , r = cast (cong (λ x → IsJust (fst x)) eq1) (eqIsJust (≡ℕ→≡ {x} {y} e))
