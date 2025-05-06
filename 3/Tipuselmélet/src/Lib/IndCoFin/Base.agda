{-# OPTIONS --safe --without-K #-}

module Lib.IndCoFin.Base where

open import Lib.Containers.CoVector.Type
open import Lib.Nat.Type
open import Lib.CoNat.Type
open import Lib.CoNat.Base hiding (embed)
open import Lib.CoNat.Bisimilarity
open import Lib.CoNat.Properties
open import Lib.IndCoFin.Type
open import Lib.Maybe.Type
open import Lib.Sum.Type
open import Lib.Equality.Type
open import Lib.Equality.Base using (subst ; sym ; _◾_)

open import Lib.Unit.Type -- needed in embed, instance is tt

infixl 9 _‼ⁱ_
_‼ⁱ_ : ∀{ℓ}{A : Set ℓ}{n : ℕ∞} → CoVec A n → IndCoFin n → A
cs ‼ⁱ izero = head cs
cs ‼ⁱ isuc i = tail cs ‼ⁱ i

tabulateⁱ : ∀{ℓ}{A : Set ℓ}{n : ℕ∞} → (IndCoFin n → A) → CoVec A n
head (tabulateⁱ f) = f izero
tail (tabulateⁱ f) = tabulateⁱ (λ i' → f (isuc i'))

cast : ∀{n k} → .(n ≈ℕ∞ k) → IndCoFin n → IndCoFin k
cast {n} {k} e (izero ⦃ p ⦄) = izero {k} ⦃ subst-IsNotZero∞ e p ⦄
cast {n} {k} e (isuc c) with pred∞ n in eq1 | pred∞ k in eq2 | .(prove e)
... | just x | just y | t = isuc {k} ⦃ JustIsNotZero∞′ {k} {y} ⦃ eq2 ⦄ ⦄ (cast (sym-pred∞≈predℕ∞ {x} {k} (transℕ∞' t (≡→≈ℕ∞' ⦃ sym eq2 ⦄))) c)

raise-suc : ∀{n} → IndCoFin n → IndCoFin (succ∞ n)
raise-suc {n} izero = izero
raise-suc {n} (isuc ⦃ p ⦄ i) = isuc {succ∞ n} (cast succpred (raise-suc {predℕ∞ n ⦃ p ⦄} i))

embed : (k : ℕ) → {n : ℕ∞} → .(k ℕ<ℕ∞ n) → IndCoFin n
embed zero {n} e = izero ⦃ <→IsNotZero∞ {zero} {n} ⦃ e ⦄ ⦄
embed (suc k) {n} e with pred∞ n in eq1
... | just x = cast (symℕ∞ (cong-succ∞ (≡→≈ℕ∞' ⦃ eq1 ⦄))) (raise-suc (embed k {x} e))

‼-tab-inv : ∀{ℓ}{A : Set ℓ}{n : ℕ∞}(i : IndCoFin n)(f : IndCoFin n → A) → tabulateⁱ f ‼ⁱ i ≡ f i
‼-tab-inv {n = n} i f with pred∞ n in eq1
‼-tab-inv {n = n} izero f | x = refl
‼-tab-inv {n = n} (isuc ⦃ p ⦄ i) f | nothing with .(subst (λ a → IsNotZero∞ (conat' a)) eq1 p)
‼-tab-inv {n = n} (isuc ⦃ p ⦄ i) f | nothing | ()
‼-tab-inv {A = A} {n = n} (isuc ⦃ p ⦄ i) f | just x = ‼-tab-inv {A = A} {predℕ∞ n} i (λ i' → f (isuc i'))
