{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoNat.Properties where

open import Lib.CoNat.PatternSynonym
open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.CoNat.Literals
open import Lib.CoNat.Bisimilarity
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Nat.Type
open import Lib.Empty.Type
open import Lib.Empty.Base
open import Lib.Dec.Type
open import Lib.Sum.Type

open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Maybe.Properties

instance
  isPropIsNotZero∞ : {n : ℕ∞} → ⦃ p1 p2 : IsNotZero∞ n ⦄ → p1 ≡ p2
  isPropIsNotZero∞ {n} with pred∞ n
  ... | just x = refl

  isNotZeroPredIsJust : {n : ℕ∞} → .⦃ IsNotZero∞ n ⦄ → IsJust (pred∞ n)
  isNotZeroPredIsJust {n} with pred∞ n
  ... | just x = tt

  succIsNotZero∞ : {n : ℕ∞} → IsNotZero∞ (succ∞ n)
  succIsNotZero∞ {n} = tt

subst-IsNotZero∞ : ∀ {n k} → .(n ≈ℕ∞ k) → .(IsNotZero∞ n) → IsNotZero∞ k
subst-IsNotZero∞ {n} {k} e _ with pred∞ n | pred∞ k | .(prove e)
... | suc∞ x | suc∞ y | _ = tt

reflℕ∞ : (x : ℕ∞) → x ≈ℕ∞ x
reflℕ∞' : (x : Maybe ℕ∞) → x ≈ℕ∞′ x
reflℕ∞' zero∞ = nothing-refl
reflℕ∞' (suc∞ x) = cong-just (reflℕ∞ x)
prove (reflℕ∞ x) = reflℕ∞' (pred∞ x)

symℕ∞ : {x y : ℕ∞} → x ≈ℕ∞ y → y ≈ℕ∞ x
symℕ∞' : {x y : Maybe ℕ∞} → x ≈ℕ∞′ y → y ≈ℕ∞′ x
symℕ∞' {zero∞} {zero∞} e = nothing-refl
symℕ∞' {suc∞ x} {suc∞ y} (cong-just e) = cong-just (symℕ∞ e)
prove (symℕ∞ {x} {y} e) = symℕ∞' (prove e)

transℕ∞ : {x y z : ℕ∞} → x ≈ℕ∞ y → y ≈ℕ∞ z → x ≈ℕ∞ z
transℕ∞' : {x y z : Maybe ℕ∞} → x ≈ℕ∞′ y → y ≈ℕ∞′ z → x ≈ℕ∞′ z
transℕ∞' {zero∞} {zero∞} {z} e1 e2 = e2
transℕ∞' {suc∞ x} {suc∞ y} {suc∞ z} (cong-just e₁) (cong-just e₂) = cong-just (transℕ∞ e₁ e₂)
prove (transℕ∞ e1 e2) = transℕ∞' (prove e1) (prove e2)

cong-pred∞ : {x y : ℕ∞} → x ≈ℕ∞ y → pred∞ x ≈ℕ∞′ pred∞ y
cong-pred∞ {x} {y} = prove

Conat-η : ∀{n} → conat' (pred∞ n) ≈ℕ∞ n
prove (Conat-η {n}) = reflℕ∞' (pred∞ n)

predsucc : ∀{n} → predℕ∞ (succ∞ n) ≈ℕ∞ n
predsucc {n} = reflℕ∞ n

succpred : ∀{n} → .⦃ e : IsNotZero∞ n ⦄ → succ∞ (predℕ∞ n ⦃ e ⦄) ≈ℕ∞ n
prove (succpred {n} ⦃ e ⦄) with pred∞ n
... | suc∞ x = reflℕ∞' (just x)

instance
  ≡→≈ℕ∞ : {a b : ℕ∞} → .⦃ a ≡ b ⦄ → a ≈ℕ∞ b
  ≡→≈ℕ∞' : {a b : Maybe ℕ∞} → .⦃ a ≡ b ⦄ → a ≈ℕ∞′ b
  ≡→≈ℕ∞' {zero∞} {zero∞} ⦃ e ⦄ = nothing-refl
  ≡→≈ℕ∞' {suc∞ a} {suc∞ b} ⦃ e ⦄ = cong-just (≡→≈ℕ∞ {a} {b} ⦃ just-injective e ⦄)
  prove (≡→≈ℕ∞ {a} {b} ⦃ e ⦄) = ≡→≈ℕ∞' {pred∞ a} {pred∞ b} ⦃ cong pred∞ e ⦄

  <→IsNotZero∞ : {n : ℕ}{k : ℕ∞} → .⦃ n ℕ<ℕ∞ k ⦄ → IsNotZero∞ k
  <→IsNotZero∞ {n} {k} with pred∞ k
  ... | suc∞ x = tt

pred∞-injective≡ : ∀{n k} → .(pred∞ n ≡ pred∞ k) → n ≈ℕ∞ k
prove (pred∞-injective≡ {n} {k} e) = ≡→≈ℕ∞' ⦃ e ⦄

just-injectiveℕ∞ : ∀{n k} → just n ≈ℕ∞′ just k → n ≈ℕ∞ k
just-injectiveℕ∞ {n} {k} (cong-just e) = e

just→pred∞ : ∀{n k} → just n ≈ℕ∞′ just k → pred∞ n ≈ℕ∞′ pred∞ k
just→pred∞ {n} {k} (cong-just e) = prove e

pred∞-injective : ∀{n k} → .(pred∞ n ≈ℕ∞′ pred∞ k) → n ≈ℕ∞ k
prove (pred∞-injective {n} {k} e) with pred∞ n | pred∞ k
... | zero∞   | zero∞   = nothing-refl
... | suc∞ n' | suc∞ k' = cong-just (pred∞-injective {n'} {k'} (just→pred∞ e))

0=0 : 0 ≈ℕ∞ pred∞'' zero∞
prove 0=0 = nothing-refl

∞+1≡∞ : succ∞ ∞ ≈ℕ∞ ∞
prove ∞+1≡∞ = reflℕ∞' (just ∞)

+-aux-inr : ∀ w k → coite (+-aux w) (inr k) ≈ℕ∞ k
prove (+-aux-inr w k) with pred∞ k
... | zero∞ = nothing-refl
... | suc∞ k' = cong-just (+-aux-inr w k')

n+∞≡∞ : ∀ n → n + ∞ ≈ℕ∞ ∞
prove (n+∞≡∞ n) with pred∞ n
... | zero∞ = cong-just (+-aux-inr ∞ ∞)
... | suc∞ x = cong-just (n+∞≡∞ x)

∞+n≡∞ : ∀ n → ∞ + n ≈ℕ∞ ∞
prove (∞+n≡∞ n) = cong-just (∞+n≡∞ n)

idl+ : (n : ℕ∞) → 0 + n ≈ℕ∞ n
prove (idl+ n) with pred∞ n
... | zero∞   = nothing-refl
... | suc∞ n' = cong-just (+-aux-inr n n')

idr+ : ∀ n → n + 0 ≈ℕ∞ n
prove (idr+ n) with pred∞ n
... | zero∞ = nothing-refl
... | suc∞ n' = cong-just (idr+ n')

weakenℕ∞ : (a b : ℕ∞) → .(e : IsNotZero∞ a) → IsNotZero∞ (a + b)
weakenℕ∞ a b e with pred∞ a
... | suc∞ _ = tt

instance
  JustIsNotZero∞ : {n n' : ℕ∞} → .⦃ pred∞ n ≈ℕ∞′ just n' ⦄ → IsNotZero∞ n
  JustIsNotZero∞ {n} with pred∞ n
  ... | suc∞ _ = tt

  JustIsNotZero∞′ : {n n' : ℕ∞} → .⦃ pred∞ n ≡ just n' ⦄ → IsNotZero∞ n
  JustIsNotZero∞′ {n} = JustIsNotZero∞ {n}

+-injectiveʳ : (a b c : ℕ∞) → a ≈ℕ∞ b → a + c ≈ℕ∞ b + c
prove (+-injectiveʳ a b c e) with pred∞ a in eq1 | pred∞ b in eq2
prove (+-injectiveʳ a b c e) | zero∞ | zero∞ = reflℕ∞' _
prove (+-injectiveʳ a b c e) | zero∞ | suc∞ b' with transℕ∞' (≡→≈ℕ∞' ⦃ sym eq1 ⦄) (transℕ∞' (prove e) (≡→≈ℕ∞' ⦃ eq2 ⦄))
... | ()
prove (+-injectiveʳ a b c e) | suc∞ a' | zero∞ with transℕ∞' (≡→≈ℕ∞' ⦃ sym eq1 ⦄) (transℕ∞' (prove e) (≡→≈ℕ∞' ⦃ eq2 ⦄))
... | ()
prove (+-injectiveʳ a b c e) | suc∞ a' | suc∞ b' with transℕ∞' (≡→≈ℕ∞' ⦃ sym eq1 ⦄) (transℕ∞' (prove e) (≡→≈ℕ∞' ⦃ eq2 ⦄))
... | cong-just e' = cong-just (+-injectiveʳ a' b' c e')

+-injectiveˡ : (a b c : ℕ∞) → a ≈ℕ∞ b → c + a ≈ℕ∞ c + b
prove (+-injectiveˡ a b c e) with pred∞ c in eq-c
prove (+-injectiveˡ a b c e) | zero∞ with pred∞ a in eq-a | pred∞ b in eq-b
prove (+-injectiveˡ a b c e) | zero∞ | zero∞ | zero∞ = nothing-refl
prove (+-injectiveˡ a b c e) | zero∞ | zero∞ | suc∞ b' with transℕ∞' (≡→≈ℕ∞' ⦃ sym eq-a ⦄) (transℕ∞' (prove e) (≡→≈ℕ∞' ⦃ eq-b ⦄))
... | ()
prove (+-injectiveˡ a b c e) | zero∞ | suc∞ a' | zero∞ with transℕ∞' (≡→≈ℕ∞' ⦃ sym eq-a ⦄) (transℕ∞' (prove e) (≡→≈ℕ∞' ⦃ eq-b ⦄))
... | ()
prove (+-injectiveˡ a b c e) | zero∞ | suc∞ a' | suc∞ b' with transℕ∞' (≡→≈ℕ∞' ⦃ sym eq-a ⦄) (transℕ∞' (prove e) (≡→≈ℕ∞' ⦃ eq-b ⦄))
... | cong-just e' = cong-just (transℕ∞ (+-aux-inr a a') (transℕ∞ e' (symℕ∞ (+-aux-inr b b'))))
prove (+-injectiveˡ a b c e) | suc∞ c' = cong-just (+-injectiveˡ a b c' e)

pred∞≡predℕ∞ : ∀{n x}(e : pred∞ n ≡ just x) → predℕ∞ n ⦃ JustIsNotZero∞′ {n} {x} ⦃ e ⦄ ⦄ ≡ x
pred∞≡predℕ∞ {n} {x} e with pred∞ n
... | just n' = just-injective e

pred∞≈predℕ∞ : ∀{n x}(e : pred∞ n ≈ℕ∞′ just x) → predℕ∞ n ⦃ JustIsNotZero∞ {n} ⦃ e ⦄ ⦄ ≈ℕ∞ x
prove (pred∞≈predℕ∞ {n} {x} e) with pred∞ n
... | suc∞ y = just→pred∞ e

sym-pred∞≈predℕ∞ : ∀{x n}(e : just x ≈ℕ∞′ pred∞ n) → x ≈ℕ∞ predℕ∞ n ⦃ JustIsNotZero∞ {n} ⦃ symℕ∞' e ⦄ ⦄
prove (sym-pred∞≈predℕ∞ {x} {n} e) with pred∞ n
... | suc∞ y = just→pred∞ e

cong-succ∞ : ∀{n x} → pred∞ n ≈ℕ∞′ just x → n ≈ℕ∞ succ∞ x
prove (cong-succ∞ {n} {x} e) with pred∞ n
... | suc∞ n' = e

{-
Fresh list

data FreshList {i j} (A : Set i) (R : A → A → Set j) : Set (i ⊔ j)
_#_ : ∀{i j}{A : Set i}{R : A → A → Set m} → (x : A) → (xs : FreshList A R) → Set (i ⊔ j)

data FreshList A R where
  []     : FreshList A R
  _∷_by_ : (x : A) (xs : FreshList A R) (x#xs : x # xs) → FreshList A R

_#_ {A = A} {R = R} _ [] = ⊤
_#_ {A = A} {R = R} x (y ∷ xs by p) = R x y × x # xs
-}

{-
transℕ∞Reflˡ : {x y : ℕ∞}(e : x ≈ℕ∞ y) → transℕ∞ (reflℕ∞ x) e ≈ℕ∞ₚᵣ e
transℕ∞Reflˡ' : {x y : Maybe ℕ∞}(e : x ≈ℕ∞′′ y) → transℕ∞' (reflℕ∞' x) e ≈ℕ∞'ₚᵣ e
transℕ∞Reflˡ' {zero∞} {zero∞} e = _
transℕ∞Reflˡ' {suc∞ x} {suc∞ y} e = transℕ∞Reflˡ {x} {y} e
prove-eq (transℕ∞Reflˡ {x} {y} e) = transℕ∞Reflˡ' {pred∞ x} {pred∞ y} (prove e)

transℕ∞Reflʳ : {x y : ℕ∞}(e : x ≈ℕ∞ y) → transℕ∞ e (reflℕ∞ y) ≈ℕ∞ₚᵣ e
transℕ∞Reflʳ' : {x y : Maybe ℕ∞}(e : x ≈ℕ∞′′ y) → transℕ∞' e (reflℕ∞' y) ≈ℕ∞'ₚᵣ e
transℕ∞Reflʳ' {zero∞} {zero∞} e = _
transℕ∞Reflʳ' {suc∞ x} {suc∞ y} e = transℕ∞Reflʳ {x} {y} e
prove-eq (transℕ∞Reflʳ {x} {y} e) = transℕ∞Reflʳ' {pred∞ x} {pred∞ y} (prove e)
-}
