{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Equality.Base where

open import Lib.Nat.Equality.Type
open import Lib.Nat.Literals
open import Lib.Nat.Type

-----------------------------------------------------
-- ≡ℕ rules
-----------------------------------------------------

reflℕ : ∀ n → n ≡ℕ n
reflℕ zero    = tt
reflℕ (suc n) = reflℕ n

symℕ : ∀(n){m} → .(n ≡ℕ m) → m ≡ℕ n
symℕ zero  {zero}  _ = tt
symℕ (suc n) {suc m}   = symℕ n {m}

transℕ : ∀ n m k → .(n ≡ℕ m) → .(m ≡ℕ k) → n ≡ℕ k
transℕ zero zero zero e1 e2 = tt
transℕ (suc n) (suc m) (suc k) e1 e2 = transℕ n m k e1 e2

-- TODO: When ready, use Relation.Notation
_,_,_≡ℕ⟨_⟩_ : (x y z : ℕ) → .(x ≡ℕ y) → .(y ≡ℕ z) → x ≡ℕ z
x , y , z ≡ℕ⟨ p ⟩ q = transℕ x y z p q

-- \qed = ∎
_∎ℕ : (x : ℕ) → x ≡ℕ x
_∎ℕ = reflℕ

infixr 2 _,_,_≡ℕ⟨_⟩_
infix 3 _∎ℕ

sym≢ℕ : ∀(n){m} → .(n ≢ℕ m) → m ≢ℕ n
sym≢ℕ zero    {zero}  ()
sym≢ℕ zero    {suc m} _  = tt
sym≢ℕ (suc n) {zero}  e  = tt
sym≢ℕ (suc n) {suc m} e  = sym≢ℕ n {m} e

congℕ : (f : ℕ → ℕ) → ∀(a){b} → .(a ≡ℕ b) → f a ≡ℕ f b
congℕ f zero    {zero}  e = reflℕ (f 0)
congℕ f (suc a) {suc b} e = congℕ (λ x → f (suc x)) a {b} e

substℕ : ∀{i}(P : ℕ → Set i){x y : ℕ} → .(x ≡ℕ y) → P x → P y
substℕ P {zero} {zero} e px = px
substℕ P {suc x} {suc y} = substℕ (λ z → P (suc z)) {x} {y}

---------------------------------------------------------
-- ≤ℕ rules
---------------------------------------------------------

refl≤ℕ : (n : ℕ) → n ≤ℕ n
refl≤ℕ zero = tt
refl≤ℕ (suc n) = refl≤ℕ n

trans≤ℕ : ∀ n m k → .(n ≤ℕ m) → .(m ≤ℕ k) → n ≤ℕ k
trans≤ℕ zero m k e1 e2 = tt
trans≤ℕ (suc n) (suc m) (suc k) e1 e2 = trans≤ℕ n m k e1 e2

≡ℕ→≤ℕ : ∀ n m → .(n ≡ℕ m) → n ≤ℕ m
≡ℕ→≤ℕ zero m e = tt
≡ℕ→≤ℕ (suc n) (suc m) e = ≡ℕ→≤ℕ n m e

---------------------------------------------------------
-- <ℕ rules
---------------------------------------------------------

trans<ℕ : ∀ n m k → .(n <ℕ m) → .(m <ℕ k) → n <ℕ k
trans<ℕ zero (suc m) (suc k) e1 e2 = tt
trans<ℕ (suc n) (suc m) (suc k) e1 e2 = trans<ℕ n m k e1 e2

<ℕ→≤ℕ : ∀ n k → .(n <ℕ k) → n ≤ℕ k
<ℕ→≤ℕ zero k e = tt
<ℕ→≤ℕ (suc n) (suc k) e = <ℕ→≤ℕ n k e

≤ℕ→<ℕ : ∀ n k → .(suc n ≤ℕ k) → n <ℕ k
≤ℕ→<ℕ zero (suc k) _ = tt
≤ℕ→<ℕ (suc n) (suc k) = ≤ℕ→<ℕ n k

<ℕ→≢ℕ : ∀ n k → .(n <ℕ k) → n ≢ℕ k
<ℕ→≢ℕ zero (suc k) e = tt
<ℕ→≢ℕ (suc n) (suc k) e = <ℕ→≢ℕ n k e

---------------------------------------------------------
-- >ℕ rules
---------------------------------------------------------

trans>ℕ : ∀ n m k → .(n >ℕ m) → .(m >ℕ k) → n >ℕ k
trans>ℕ (suc n) (suc m) zero e1 e2 = tt
trans>ℕ (suc n) (suc m) (suc k) e1 e2 = trans>ℕ n m k e1 e2

>ℕ→<ℕ : ∀ n m → .(n >ℕ m) → m <ℕ n
>ℕ→<ℕ (suc n) zero e = tt
>ℕ→<ℕ (suc n) (suc m) e = >ℕ→<ℕ n m e

<ℕ→>ℕ : ∀ n m → .(n <ℕ m) → m >ℕ n
<ℕ→>ℕ zero (suc m) e = tt
<ℕ→>ℕ (suc n) (suc m) e = <ℕ→>ℕ n m e

>ℕ→≢ℕ : ∀ n k → .(n >ℕ k) → n ≢ℕ k
>ℕ→≢ℕ zero (suc n) e = tt
>ℕ→≢ℕ (suc n) zero e = tt
>ℕ→≢ℕ (suc n) (suc k) e = >ℕ→≢ℕ n k e

---------------------------------------------------------
-- ≥ℕ rules
---------------------------------------------------------

refl≥ℕ : (n : ℕ) → n ≥ℕ n
refl≥ℕ zero = tt
refl≥ℕ (suc n) = refl≥ℕ n

trans≥ℕ : ∀ n m k → .(n ≥ℕ m) → .(m ≥ℕ k) → n ≥ℕ k
trans≥ℕ n m zero e1 e2 = tt
trans≥ℕ (suc n) (suc m) (suc k) e1 e2 = trans≥ℕ n m k e1 e2

>ℕ→≥ℕ : ∀ n m → .(n >ℕ m) → n ≥ℕ m
>ℕ→≥ℕ n zero e = tt
>ℕ→≥ℕ (suc n) (suc m) e = >ℕ→≥ℕ n m e

≥ℕ→>ℕ : ∀ n m → .(n ≥ℕ suc m) → n >ℕ m
≥ℕ→>ℕ (suc n) zero e = tt
≥ℕ→>ℕ (suc n) (suc m) e = ≥ℕ→>ℕ n m e

≥ℕ→≤ℕ : ∀ n m → .(n ≥ℕ m) → m ≤ℕ n
≥ℕ→≤ℕ n zero e = tt
≥ℕ→≤ℕ (suc n) (suc m) e = ≥ℕ→≤ℕ n m e

≤ℕ→≥ℕ : ∀ n m → .(n ≤ℕ m) → m ≥ℕ n
≤ℕ→≥ℕ zero m e = tt
≤ℕ→≥ℕ (suc n) (suc m) e = ≤ℕ→≥ℕ n m e

≡ℕ→≥ℕ : ∀ n m → .(n ≡ℕ m) → n ≥ℕ m
≡ℕ→≥ℕ n zero e = tt
≡ℕ→≥ℕ (suc n) (suc m) e = ≡ℕ→≥ℕ n m e
