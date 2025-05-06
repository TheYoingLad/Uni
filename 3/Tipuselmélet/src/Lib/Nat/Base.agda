{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Base where

open import Lib.Nat.Literals
open import Lib.Nat.Type
open import Lib.Nat.Equality.Type
open import Lib.Nat.Equality.Base
open import Lib.Empty.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
import Lib.Sum.Base as Sum
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Containers.List.Type
open import Lib.UnitOrEmpty.Type
open import Agda.Builtin.Nat public
  hiding (Nat ; suc ; zero)
  renaming (_<_ to _<ᵇ_ ; _==_ to _==ᵇ_ ; _-_ to _-'_)

IsZero : ℕ → Set
IsZero 0 = ⊤
IsZero (suc _) = ⊥

IsNotZero : ℕ → Set
IsNotZero 0 = ⊥
IsNotZero (suc _) = ⊤

pred' : ℕ → ℕ
pred' 0 = 0
pred' (suc n) = n

pred : (n : ℕ) → .⦃ nonZero : IsNotZero n ⦄ → ℕ
pred (suc n) = n

infixr 6 _-_
_-_ : (x y : ℕ) → .⦃ nonZero : x ≥ℕ y ⦄ → ℕ
zero - zero = zero
suc x - zero = suc x
suc x - suc y = x - y

infixr 8 _^_
_^_ : ℕ → ℕ → ℕ
x ^ zero  = 1
x ^ suc n = x * x ^ n

infix 8 _⁰ _¹ _² _³ _⁴ _⁵ _⁶ _⁷ _⁸ _⁹

_⁰ : ℕ → ℕ
_⁰ = _^ 0

_¹ : ℕ → ℕ
_¹ = _^ 1

_² : ℕ → ℕ
_² = _^ 2

_³ : ℕ → ℕ
_³ = _^ 3

_⁴ : ℕ → ℕ
_⁴ = _^ 4

_⁵ : ℕ → ℕ
_⁵ = _^ 5

_⁶ : ℕ → ℕ
_⁶ = _^ 6

_⁷ : ℕ → ℕ
_⁷ = _^ 7

_⁸ : ℕ → ℕ
_⁸ = _^ 8

_⁹ : ℕ → ℕ
_⁹ = _^ 9

infixl 50 _!
_! : ℕ → ℕ
zero  ! = 1
suc n ! = n ! * suc n

infixl 7 _div_
_div_ : ℕ → ℕ → ℕ
n div 1+m = div-helper 0 1+m n 1+m

infixl 7 _mod_
_mod_ : ℕ → ℕ → ℕ
n mod 1+m = mod-helper 0 1+m n 1+m

{-
Evenᵗ : ℕ → ⊤or⊥
Evenᵗ 0 = ⊤ , inl refl
Evenᵗ 1 = ⊥ , inr refl
Evenᵗ (suc (suc n)) = Evenᵗ n

Even : ℕ → Set
Even n = fst (Evenᵗ n)

Oddᵗ : ℕ → ⊤or⊥
Oddᵗ 0 = ⊥ , inr refl
Oddᵗ 1 = ⊤ , inl refl
Oddᵗ (suc (suc n)) = Oddᵗ n

Odd : ℕ → Set
Odd n = fst (Oddᵗ n)
-}

data Even : ℕ → Set where
  instance Even0 : Even 0
  instance Even+2 : {n : ℕ} → .⦃ Even n ⦄ → Even (suc (suc n))

data Odd : ℕ → Set where
  instance Odd1 : Odd 1
  instance Odd+2 : {n : ℕ} → .⦃ Odd n ⦄ → Odd (suc (suc n))

case : ∀{i}{A : Set i} → ℕ → A → A → A
case zero    z s = z
case (suc _) z s = s

case-proof : ∀{i}{A : ℕ → Set i}(n : ℕ) → (n ≡ 0 → A 0) → ({k : ℕ} → n ≡ suc k → A (suc k)) → A n
case-proof zero a0 ak = a0 refl
case-proof (suc n) a0 ak = ak refl

ite : ∀{i}{A : Set i} → ℕ → A → (A → A) → A
ite zero z s = z
ite (suc n) z s = s (ite n z s)

rec : ∀{i}{A : Set i} → ℕ → A → (ℕ → A → A) → A
rec zero    z s = z
rec (suc n) z s = s n (rec n z s)

elim : ∀{i}{A : ℕ → Set i} → (n : ℕ) → A 0 → ({k : ℕ} → A k → A (suc k)) → A n
elim         zero    a0 f = a0
elim {A = A} (suc n) a0 f = f (elim {A = A} n a0 f)

minMax : (n k : ℕ) → Σ (ℕ × ℕ) (λ {(a , b) → (n ≤ℕ k × n ≡ℕ a × k ≡ℕ b) ⊎ (k ≤ℕ n × k ≡ℕ a × n ≡ℕ b)})
minMax zero k = (zero , k) , inl (tt , tt , reflℕ k)
minMax (suc n) zero = (zero , suc n) , inr (tt , tt , reflℕ n)
minMax (suc n) (suc k) = let ((a , b) , c) = minMax n k in (suc a , suc b) , c

minWithProof : (n k : ℕ) → Σ ℕ (λ a → (n ≤ℕ k × n ≡ℕ a) ⊎ (k ≤ℕ n × k ≡ℕ a))
minWithProof n k = let ((a , b) , e) = minMax n k in Sum.case e (λ (e1 , e2 , e3) → a , inl (e1 , e2)) (λ (e1 , e2 , e3) → a , inr (e1 , e2))

maxWithProof : (n k : ℕ) → Σ ℕ (λ a → (n ≤ℕ k × k ≡ℕ a) ⊎ (k ≤ℕ n × n ≡ℕ a))
maxWithProof n k = let ((a , b) , e) = minMax n k in Sum.case e (λ (e1 , e2 , e3) → b , inl (e1 , e3)) (λ (e1 , e2 , e3) → b , inr (e1 , e3))

min : ℕ → ℕ → ℕ
min n m = fst (fst (minMax n m))

max : ℕ → ℕ → ℕ
max n m = snd (fst (minMax n m))
