{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Equality.Properties where

open import Lib.Nat.Literals
open import Lib.Nat.Equality.Type
open import Lib.Nat.Equality.Base
open import Lib.Nat.Base
open import Lib.Nat.Type

suc-injective≡ℕ : ∀{n m} → suc n ≡ℕ suc m → n ≡ℕ m
suc-injective≡ℕ e = e

suc-injective≢ℕ : ∀{n m} → suc n ≢ℕ suc m → n ≢ℕ m
suc-injective≢ℕ e = e

pred-injective≡ℕ : ∀{n m} → .⦃ eq1 : IsNotZero n ⦄ → .⦃ eq2 : IsNotZero m ⦄ → pred n ≡ℕ pred m → n ≡ℕ m
pred-injective≡ℕ {suc n} {suc m} e = e

0≢ℕsucn : ∀ {n} → 0 ≢ℕ suc n
0≢ℕsucn = tt

sucn≢ℕ0 : ∀ {n} → suc n ≢ℕ 0
sucn≢ℕ0 = tt

sucn≢ℕn : ∀ {n} → suc n ≢ℕ n
sucn≢ℕn {zero} = tt
sucn≢ℕn {suc n} = sucn≢ℕn {n}

n≢ℕsucn : ∀ {n} → n ≢ℕ suc n
n≢ℕsucn {zero} = tt
n≢ℕsucn {suc n} = n≢ℕsucn {n}

idr+ℕ : (n : ℕ) → n + zero ≡ℕ n
idr+ℕ zero = tt
idr+ℕ (suc n) = idr+ℕ n

sucr+ℕ : (n m : ℕ) → n + suc m ≡ℕ suc (n + m)
sucr+ℕ zero m = reflℕ m
sucr+ℕ (suc n) m = sucr+ℕ n m

ass+ℕ : (m n o : ℕ) → (m + n) + o ≡ℕ m + (n + o)
ass+ℕ zero n o = reflℕ (n + o)
ass+ℕ (suc m) n o = ass+ℕ m n o

comm+ℕ : (m n : ℕ) → m + n ≡ℕ n + m
comm+ℕ zero n = symℕ (n + 0) ((idr+ℕ n))
comm+ℕ (suc m) n = 
  suc m + n , suc (n + m) , n + suc m
  ≡ℕ⟨ congℕ suc (m + n) (comm+ℕ m n) ⟩ 
  suc n + m , n + suc m , _
  ≡ℕ⟨ symℕ (n + suc m) (sucr+ℕ n m) ⟩ 
  n + suc m ∎ℕ

dist+*ℕ : (m n o : ℕ) → (m + n) * o ≡ℕ m * o + n * o
dist+*ℕ zero n o = reflℕ (n * o)
dist+*ℕ (suc m) n o =
  transℕ (o + (m + n) * o) (o + (m * o + n * o)) _ 
    (congℕ (o +_) ((m + n) * o) (dist+*ℕ m n o))
    (symℕ (o + m * o + n * o) (ass+ℕ o (m * o) (n * o)))

{-
dist*+ : (m n o : ℕ) → m * (n + o) ≡ m * n + m * o
dist*+ zero n o = refl
dist*+ (suc m) n o =
  n + o + m * (n + o)
  ≡⟨ ass+ n o (m * (n + o)) ⟩
  n + (o + m * (n + o)) 
  ≡⟨ cong (n +_) (comm+ o (m * (n + o))) ⟩
  n + (m * (n + o) + o)
  ≡⟨ sym (ass+ n (m * (n + o)) o) ⟩
  n + m * (n + o) + o
  ≡⟨ cong (λ x → n + x + o) (dist*+ m n o) ⟩
  n + (m * n + m * o) + o
  ≡⟨ cong (_+ o) (sym (ass+ n (m * n) (m * o))) ⟩
  n + m * n + m * o + o
  ≡⟨ ass+ (n + m * n) (m * o) o ⟩
  n + m * n + (m * o + o)
  ≡⟨ cong ((n + m * n) +_) (comm+ (m * o) o) ⟩
  n + m * n + (o + m * o) ∎
-}

nullr*ℕ : (n : ℕ) → n * 0 ≡ℕ 0
nullr*ℕ zero = tt
nullr*ℕ (suc n) = nullr*ℕ n

idl*ℕ : (n : ℕ) → 1 * n ≡ℕ n
idl*ℕ = idr+ℕ

idr*ℕ : (n : ℕ) → n * 1 ≡ℕ n
idr*ℕ zero = tt
idr*ℕ (suc n) = congℕ suc (n * 1) (idr*ℕ n)

sucr*ℕ : (n m : ℕ) → n * suc m ≡ℕ n + n * m
sucr*ℕ zero m = tt
sucr*ℕ (suc n) m =
  m + n * suc m , m + (n + n * m) , _
  ≡ℕ⟨ congℕ (m +_) _ (sucr*ℕ n m) ⟩
  m + (n + n * m) , m + n + n * m , _
  ≡ℕ⟨ symℕ (m + n + n * m) (ass+ℕ m n (n * m)) ⟩
  m + n + n * m , n + m + n * m , _
  ≡ℕ⟨ congℕ (_+ n * m) (m + n) (comm+ℕ m n) ⟩
  n + m + n * m , n + (m + n * m) , _
  ≡ℕ⟨ ass+ℕ n m (n * m) ⟩
  n + (m + n * m) ∎ℕ

ass*ℕ : (m n o : ℕ) → (m * n) * o ≡ℕ m * (n * o)
ass*ℕ zero n o = tt
ass*ℕ (suc m) n o =
  (n + m * n) * o , _ , _
  ≡ℕ⟨ dist+*ℕ n (m * n) o ⟩
  n * o + m * n * o , _ , _
  ≡ℕ⟨ congℕ (n * o +_) _ (ass*ℕ m n o) ⟩
  n * o + m * (n * o) ∎ℕ

comm*ℕ : (m n : ℕ) → m * n ≡ℕ n * m
comm*ℕ zero n = symℕ (n * 0) (nullr*ℕ n)
comm*ℕ (suc m) n =
  n + m * n , _ , _
  ≡ℕ⟨ congℕ (n +_) _ (comm*ℕ m n) ⟩
  n + n * m , _ , _
  ≡ℕ⟨ symℕ (n * suc m) (sucr*ℕ n m) ⟩
  n * suc m ∎ℕ

nulll^ℕ : (n : ℕ) → 1 ^ n ≡ℕ 1
nulll^ℕ zero = tt
nulll^ℕ (suc n) = transℕ (1 ^ n + 0) _ _ (idr+ℕ (1 ^ n)) (nulll^ℕ n)

idr^ℕ : (a : ℕ) → a ^ 1 ≡ℕ a
idr^ℕ = idr*ℕ

{-
dist^+ : (m n o : ℕ) → m ^ (n + o) ≡ m ^ n * m ^ o
dist^+ m zero o = sym (idr+ (m ^ o))
dist^+ m (suc n) o = trans (cong (m *_) (dist^+ m n o)) (sym (ass* m (m ^ n) (m ^ o)))
-}
{-
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
  ≡⟨ cong (a *_) (sym (ass* (a ^ n) (a ^ m) (a ^ (m * n)))) ⟩
  a * (a ^ n * a ^ m * a ^ (m * n))
  ≡⟨ cong (λ x → a * (x * a ^ (m * n))) (comm* (a ^ n) (a ^ m)) ⟩
  a * (a ^ m * a ^ n * a ^ (m * n))
  ≡⟨ cong (a *_) (ass* (a ^ m) (a ^ n) (a ^ (m * n))) ⟩
  a * (a ^ m * (a ^ n * a ^ (m * n)))
  ≡⟨ sym (ass* a (a ^ m) (a ^ n * a ^ (m * n))) ⟩
  a * a ^ m * (a ^ n * a ^ (m * n))
  ≡⟨ cong (a * a ^ m *_) (sym (dist^+ a n (m * n))) ⟩
  a * a ^ m * a ^ (n + m * n)
  ≡⟨ cong (a ^ suc m *_) (dist^* a (suc m) n) ⟩
  a ^ suc m * (a ^ suc m) ^ n ∎
-}
{-
dist*^ : (a b n : ℕ) → (a * b) ^ n ≡ a ^ n * b ^ n
dist*^ a b zero = refl
dist*^ a b (suc n) =
  a * b * (a * b) ^ n
  ≡⟨ cong (a * b *_) (dist*^ a b n) ⟩
  a * b * (a ^ n * b ^ n)
  ≡⟨ ass* a b (a ^ n * b ^ n) ⟩
  a * (b * (a ^ n * b ^ n))
  ≡⟨ cong (a *_) (sym (ass* b (a ^ n) (b ^ n))) ⟩
  a * (b * a ^ n * b ^ n)
  ≡⟨ cong (λ x → a * (x * b ^ n)) (comm* b (a ^ n)) ⟩
  a * (a ^ n * b * b ^ n)
  ≡⟨ cong (a *_) (ass* (a ^ n) b (b ^ n)) ⟩
  a * (a ^ n * (b * b ^ n))
  ≡⟨ sym (ass* a (a ^ n) (b * b ^ n)) ⟩
  a * a ^ n * (b * b ^ n) ∎
-}

----------------------
-- Properties of ≤ℕ --
----------------------

≤ℕ+r : ∀ n k → n ≤ℕ n + k
≤ℕ+r zero k = tt
≤ℕ+r (suc n) k = ≤ℕ+r n k

≤ℕ+l : ∀ n k → n ≤ℕ k + n
≤ℕ+l zero k = tt
≤ℕ+l (suc n) k = substℕ (suc n ≤ℕ_) {suc n + k} {k + suc n} (comm+ℕ (suc n) k) (≤ℕ+r n k)

monotonous+≤ℕ : ∀ a b c d → .(a ≤ℕ b) → .(c ≤ℕ d) → a + c ≤ℕ b + d
monotonous+≤ℕ zero b c d e1 e2 = trans≤ℕ c d (b + d) e2 (≤ℕ+l d b)
monotonous+≤ℕ (suc a) (suc b) c d e1 e2 = monotonous+≤ℕ a b c d e1 e2

monotonous*≤ℕ : ∀ a b c d → a ≤ℕ b → c ≤ℕ d → a * c ≤ℕ b * d
monotonous*≤ℕ zero b c d e1 e2 = tt
monotonous*≤ℕ (suc a) (suc b) c d e1 e2 = monotonous+≤ℕ c d (a * c) (b * d) e2 (monotonous*≤ℕ a b c d e1 e2)

monotonous^≤ℕ : ∀ a b c d → a ≤ℕ suc b → c ≤ℕ d → a ^ c ≤ℕ suc b ^ d
monotonous^≤ℕ a b zero zero e1 e2 = tt
monotonous^≤ℕ a b zero (suc d) e1 e2 = trans≤ℕ 1 (suc b ^ d) (suc b ^ suc d) (monotonous^≤ℕ a b zero d e1 tt) (≤ℕ+r (suc b ^ d) (b * suc b ^ d))
monotonous^≤ℕ a b (suc c) (suc d) e1 e2 = monotonous*≤ℕ a (suc b) (a ^ c) (suc b ^ d) e1 (monotonous^≤ℕ a b c d e1 e2)

-- one with equality is just subst and have P be the ≤ ≥ < > whichever you need.
trans<≤ℕ : ∀ n m k → .(n <ℕ m) → .(m ≤ℕ k) → n <ℕ k
trans<≤ℕ zero (suc m) (suc k) e1 e2 = tt
trans<≤ℕ (suc n) (suc m) (suc k) e1 e2 = trans<≤ℕ n m k e1 e2

trans≤<ℕ : ∀ n m k → .(n ≤ℕ m) → .(m <ℕ k) → n <ℕ k
trans≤<ℕ zero m (suc k) e1 e2 = tt
trans≤<ℕ (suc n) (suc m) (suc k) e1 e2 = trans≤<ℕ n m k e1 e2

----------------------
-- Properties of <ℕ --
----------------------

n<sucn : (n : ℕ) → n <ℕ suc n
n<sucn zero = tt
n<sucn (suc n) = n<sucn n

<ℕ+r : ∀ n k → n <ℕ n + suc k
<ℕ+r zero k = tt
<ℕ+r (suc n) k = <ℕ+r n k

<ℕ+l : ∀ n k → n <ℕ suc k + n
<ℕ+l zero k = tt
<ℕ+l (suc n) k = substℕ (suc n <ℕ_) {suc n + suc k} {suc k + suc n} (comm+ℕ (suc n) (suc k)) (<ℕ+r n k)

----------------------
-- Properties of >ℕ --
----------------------

sucn>n : (n : ℕ) → suc n >ℕ n
sucn>n zero = tt
sucn>n (suc n) = sucn>n n

>ℕ+r : ∀ n k → n + suc k >ℕ n
>ℕ+r zero k = tt
>ℕ+r (suc n) k = >ℕ+r n k

>ℕ+l : ∀ n k → suc k + n >ℕ n
>ℕ+l zero k = tt
>ℕ+l (suc n) k = substℕ (_>ℕ suc n) {suc n + suc k} {suc k + suc n} (comm+ℕ (suc n) (suc k)) (>ℕ+r n k)

----------------------
-- Properties of ≥ℕ --
----------------------

≥ℕ+r : ∀ n k → n + k ≥ℕ n
≥ℕ+r zero k = tt
≥ℕ+r (suc n) k = ≥ℕ+r n k

≥ℕ+l : ∀ n k → k + n ≥ℕ n
≥ℕ+l zero k = tt
≥ℕ+l (suc n) k = substℕ (_≥ℕ suc n) {suc n + k} {k + suc n} (comm+ℕ (suc n) k) (≥ℕ+r n k)

trans>≥ℕ : ∀ n m k → .(n >ℕ m) → .(m ≥ℕ k) → n >ℕ k
trans>≥ℕ (suc n) m zero e1 e2 = tt
trans>≥ℕ (suc n) (suc m) (suc k) e1 e2 = trans>≥ℕ n m k e1 e2

trans≥>ℕ : ∀ n m k → .(n ≥ℕ m) → .(m >ℕ k) → n >ℕ k
trans≥>ℕ (suc n) (suc m) zero e1 e2 = tt
trans≥>ℕ (suc n) (suc m) (suc k) e1 e2 = trans≥>ℕ n m k e1 e2
