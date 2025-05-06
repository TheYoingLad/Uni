{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Properties where

open import Lib.Nat.Type
open import Lib.Nat.Base renaming (case to case-ℕ)
open import Lib.Unit.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties
open import Lib.Dec.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
open import Lib.Sum.Base
open import Lib.Relation.Base
open import Lib.Relation.Notation {A = ℕ} _≡_ trans refl

open import Lib.Isomorphism.Type

sucpred' : (n : ℕ) → .⦃ IsNotZero n ⦄ → suc (pred' n) ≡ n
sucpred' (suc n) = refl

cong-sucpred' : ∀{n m}(e : suc n ≡ suc m) → cong (λ x → suc (pred' x)) e ≡ e
cong-sucpred' refl = refl

suc-injective : ∀{n m} → suc n ≡ suc m → n ≡ m
suc-injective = cong pred'

pred-injective : ∀{n m} → .⦃ eq1 : IsNotZero n ⦄ → .⦃ eq2 : IsNotZero m ⦄ → pred n ≡ pred m → n ≡ m
pred-injective {suc n} {suc m} = cong suc

suc-injective-injective : ∀{n m}(e e' : suc n ≡ suc m) → suc-injective e ≡ suc-injective e' → e ≡ e'
suc-injective-injective refl e' t = cong (cong suc) t ◾ cong∘ suc pred' e' ◾ cong-sucpred' e'

sucEqIso : ∀ n m → (suc n ≡ suc m) ≅ (n ≡ m)
sucEqIso n m = ≅-proof suc-injective (cong suc) (λ e → cong∘ suc pred' e ◾ cong-sucpred' e) λ {refl → refl}

0≢sucn : ∀ {n} → 0 ≢ suc n
0≢sucn ()

sucn≢0 : ∀ {n} → suc n ≢ 0
sucn≢0 ()

sucn≢n : ∀ {n} → suc n ≢ n
sucn≢n ()

n≢sucn : ∀ {n} → n ≢ suc n
n≢sucn ()

idr+ : (n : ℕ) → n + zero ≡ n
idr+ zero = refl
idr+ (suc n) = cong suc (idr+ n)

sucr+ : (n m : ℕ) → n + suc m ≡ suc (n + m)
sucr+ zero m = refl
sucr+ (suc n) m = cong suc (sucr+ n m)

assoc+ : (m n o : ℕ) → (m + n) + o ≡ m + (n + o)
assoc+ zero n o = refl
assoc+ (suc m) n o = cong suc (assoc+ m n o)

comm+ : (m n : ℕ) → m + n ≡ n + m
comm+ zero n = sym (idr+ n)
comm+ (suc m) n = trans (cong suc (comm+ m n)) (sym (sucr+ n m))

dist+* : (m n o : ℕ) → (m + n) * o ≡ m * o + n * o
dist+* zero n o = refl
dist+* (suc m) n o = trans (cong (o +_) (dist+* m n o)) (sym (assoc+ o (m * o) (n * o)))

dist*+ : (m n o : ℕ) → m * (n + o) ≡ m * n + m * o
dist*+ zero n o = refl
dist*+ (suc m) n o =
  n + o + m * (n + o)
  ≡⟨ assoc+ n o (m * (n + o)) ⟩
  n + (o + m * (n + o))
  ≡⟨ cong (n +_) (comm+ o (m * (n + o))) ⟩
  n + (m * (n + o) + o)
  ≡⟨ sym (assoc+ n (m * (n + o)) o) ⟩
  n + m * (n + o) + o
  ≡⟨ cong (λ x → n + x + o) (dist*+ m n o) ⟩
  n + (m * n + m * o) + o
  ≡⟨ cong (_+ o) (sym (assoc+ n (m * n) (m * o))) ⟩
  n + m * n + m * o + o
  ≡⟨ assoc+ (n + m * n) (m * o) o ⟩
  n + m * n + (m * o + o)
  ≡⟨ cong ((n + m * n) +_) (comm+ (m * o) o) ⟩
  n + m * n + (o + m * o) ∎

nullr* : (n : ℕ) → n * 0 ≡ 0
nullr* zero = refl
nullr* (suc n) = nullr* n

idl* : (n : ℕ) → 1 * n ≡ n
idl* = idr+ 

idr* : (n : ℕ) → n * 1 ≡ n
idr* zero = refl
idr* (suc n) = cong suc (idr* n)

sucr* : (n m : ℕ) → n * suc m ≡ n + n * m
sucr* zero m = refl
sucr* (suc n) m =
  suc (m + n * suc m)
  ≡⟨ cong suc (comm+ m (n * suc m)) ⟩
  suc (n * suc m + m)
  ≡⟨ cong (λ x → suc (x + m)) (sucr* n m) ⟩
  suc (n + n * m + m)
  ≡⟨ cong suc (assoc+ n (n * m) m) ⟩
  suc (n + (n * m + m))
  ≡⟨ cong (λ x → suc (n + x)) (comm+ (n * m) m) ⟩
  suc (n + (m + n * m)) ∎

assoc* : (m n o : ℕ) → (m * n) * o ≡ m * (n * o)
assoc* zero n o = refl
assoc* (suc m) n o =
  (n + m * n) * o
  ≡⟨ dist+* n (m * n) o ⟩
  n * o + m * n * o
  ≡⟨ cong (n * o +_) (assoc* m n o) ⟩
  n * o + m * (n * o) ∎

comm* : (m n : ℕ) → m * n ≡ n * m
comm* zero n = sym (nullr* n)
comm* (suc m) n =
  n + m * n
  ≡⟨ cong (n +_) (comm* m n) ⟩
  n + n * m
  ≡⟨ sym (sucr* n m) ⟩
  n * suc m ∎

nulll^ : (n : ℕ) → 1 ^ n ≡ 1
nulll^ zero = refl
nulll^ (suc n) = trans (idr+ (1 ^ n)) (nulll^ n)

idr^ : (a : ℕ) → a ^ 1 ≡ a
idr^ = idr*

dist^+ : (m n o : ℕ) → m ^ (n + o) ≡ m ^ n * m ^ o
dist^+ m zero o = sym (idr+ (m ^ o))
dist^+ m (suc n) o = trans (cong (m *_) (dist^+ m n o)) (sym (assoc* m (m ^ n) (m ^ o)))

dist^* : (a m n : ℕ) → a ^ (m * n) ≡ (a ^ m) ^ n
dist^* a 0 n = sym (nulll^ n)
dist^* a (suc m) zero = cong (a ^_) (nullr* m)
dist^* a (suc m) (suc n) =
  a * a ^ (n + m * suc n)
  ≡⟨ cong (a *_) (dist^+ a n (m * suc n)) ⟩
  a * (a ^ n * a ^ (m * suc n))
  ≡⟨ cong (λ x → a * (a ^ n * x)) (dist^* a m (suc n)) ⟩
  a * (a ^ n * (a ^ m * (a ^ m) ^ n))
  ≡⟨ cong (λ x → a * (a ^ n * (a ^ m * x))) (sym (dist^* a m n)) ⟩
  a * (a ^ n * (a ^ m * a ^ (m * n)))
  ≡⟨ cong (a *_) (sym (assoc* (a ^ n) (a ^ m) (a ^ (m * n)))) ⟩
  a * (a ^ n * a ^ m * a ^ (m * n))
  ≡⟨ cong (λ x → a * (x * a ^ (m * n))) (comm* (a ^ n) (a ^ m)) ⟩
  a * (a ^ m * a ^ n * a ^ (m * n))
  ≡⟨ cong (a *_) (assoc* (a ^ m) (a ^ n) (a ^ (m * n))) ⟩
  a * (a ^ m * (a ^ n * a ^ (m * n)))
  ≡⟨ sym (assoc* a (a ^ m) (a ^ n * a ^ (m * n))) ⟩
  a * a ^ m * (a ^ n * a ^ (m * n))
  ≡⟨ cong (a * a ^ m *_) (sym (dist^+ a n (m * n))) ⟩
  a * a ^ m * a ^ (n + m * n)
  ≡⟨ cong (a ^ suc m *_) (dist^* a (suc m) n) ⟩
  a ^ suc m * (a ^ suc m) ^ n ∎

dist*^ : (a b n : ℕ) → (a * b) ^ n ≡ a ^ n * b ^ n
dist*^ a b zero = refl
dist*^ a b (suc n) =
  a * b * (a * b) ^ n
  ≡⟨ cong (a * b *_) (dist*^ a b n) ⟩
  a * b * (a ^ n * b ^ n)
  ≡⟨ assoc* a b (a ^ n * b ^ n) ⟩
  a * (b * (a ^ n * b ^ n))
  ≡⟨ cong (a *_) (sym (assoc* b (a ^ n) (b ^ n))) ⟩
  a * (b * a ^ n * b ^ n)
  ≡⟨ cong (λ x → a * (x * b ^ n)) (comm* b (a ^ n)) ⟩
  a * (a ^ n * b * b ^ n)
  ≡⟨ cong (a *_) (assoc* (a ^ n) b (b ^ n)) ⟩
  a * (a ^ n * (b * b ^ n))
  ≡⟨ sym (assoc* a (a ^ n) (b * b ^ n)) ⟩
  a * a ^ n * (b * b ^ n) ∎

open import Lib.Dec.PatternSynonym

infix 4 _≟_
_≟_ : (x y : ℕ) → Dec (x ≡ y)
_≟_ zero zero = yes refl
_≟_ zero (suc y) = no (λ ())
_≟_ (suc x) zero = no (λ ())
_≟_ (suc x) (suc y) with _≟_ x y
... | yes refl = yes refl
... | no p = no λ a → p (suc-injective a)

Odd-2 : {n : ℕ} → Odd (suc (suc n)) → Odd n
Odd-2 {zero} (Odd+2 ⦃ ⦄)
Odd-2 {suc zero} e = Odd1
Odd-2 {suc (suc n)} (Odd+2 ⦃ p ⦄) = Odd+2 ⦃ Odd-2 {n} p ⦄

Even-2 : {n : ℕ} → Even (suc (suc n)) → Even n
Even-2 {zero} _ = Even0
Even-2 {suc zero} (Even+2 ⦃ ⦄)
Even-2 {suc (suc n)} (Even+2 ⦃ p ⦄) = Even+2 ⦃ Even-2 {n} p ⦄

EvenOdd : (n : ℕ) → Even n ⊎ Odd n
EvenOdd zero = inl Even0
EvenOdd (suc zero) = inr Odd1
EvenOdd (suc (suc n)) = case (EvenOdd n) (λ even-n → inl (Even+2 ⦃ even-n ⦄)) λ odd-n → inr (Odd+2 ⦃ odd-n ⦄)

EvenConsecutive : (n : ℕ) → Even n ⊎ Even (suc n)
EvenConsecutive zero = inl Even0
EvenConsecutive (suc n) = case (EvenConsecutive n) (λ even-n → inr (Even+2 ⦃ even-n ⦄)) inl

OddConsecutive : (n : ℕ) → Odd n ⊎ Odd (suc n)
OddConsecutive zero = inr Odd1
OddConsecutive (suc n) = case (OddConsecutive n) (λ odd-n → inr (Odd+2 ⦃ odd-n ⦄)) inl

EvenOdd-suc : (n : ℕ) → Even n → Odd (suc n)
EvenOdd-suc zero e = Odd1
EvenOdd-suc (suc (suc n)) (Even+2 ⦃ p ⦄) = Odd+2 ⦃ EvenOdd-suc n p ⦄

OddEven-suc : (n : ℕ) → Odd n → Even (suc n)
OddEven-suc (suc zero) e = Even+2
OddEven-suc (suc (suc n)) (Odd+2 ⦃ p ⦄) = Even+2 ⦃ OddEven-suc n p ⦄

EvenOdd+ : (n k : ℕ) → Even n × Even k ⊎ Odd n × Odd k ↔ Even (n + k)
fst (EvenOdd+ zero k) (inl a) = snd a
fst (EvenOdd+ (suc zero) k) (inr b) = OddEven-suc k (snd b)
fst (EvenOdd+ (suc (suc n)) k) (inl (Even+2 ⦃ even-n ⦄ , even-k)) = Even+2 ⦃ fst (EvenOdd+ n k) (inl (even-n , even-k)) ⦄
fst (EvenOdd+ (suc (suc n)) k) (inr (Odd+2 ⦃ odd-n ⦄ , odd-k)) = Even+2 ⦃ fst (EvenOdd+ n k) (inr (odd-n , odd-k)) ⦄
snd (EvenOdd+ zero k) e = inl (Even0 , e)
snd (EvenOdd+ (suc zero) k) e = inr (Odd1 , Odd-2 {k} (EvenOdd-suc (suc k) e))
snd (EvenOdd+ (suc (suc n)) k) e with snd (EvenOdd+ n k) (Even-2 e)
... | inl (even-n , even-k) = inl (Even+2 ⦃ even-n ⦄ , even-k)
... | inr (odd-n , odd-k) = inr (Odd+2 ⦃ odd-n ⦄ , odd-k)

Even2* : (n : ℕ) → Even (n + n)
Even2* zero = Even0
Even2* (suc n) = subst (λ x → Even (suc x)) (sym (sucr+ n n)) (Even+2 ⦃ Even2* n ⦄)

EvenMulConsecutive : (n : ℕ) → Even (n * suc n)
EvenMulConsecutive zero = Even0
EvenMulConsecutive (suc n) = Even+2 ⦃ subst Even (assoc+ n n _ ◾ cong (n +_) (sym (sucr* n (suc n)))) (fst (EvenOdd+ (n + n) (n * suc n)) (inl (Even2* n , EvenMulConsecutive n))) ⦄
