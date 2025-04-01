{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoNat.Base where

open import Lib.CoNat.PatternSynonym
open import Lib.CoNat.Type
open import Lib.Sum.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Sigma.Type
open import Lib.Sigma.Base
open import Lib.Nat.Type
open import Lib.Equality
open import Lib.Maybe.Type
open import Lib.Maybe.Base
  renaming ( ite to ite-Maybe )

open import Lib.Bool.Type
open import Lib.Bool.Base
open import Lib.Containers.CoList.Type
open import Lib.Containers.CoList.Base

IsZero∞ᵗ : ℕ∞ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
IsZero∞ᵗ n = ite-Maybe (λ _ → ⊥ , inr refl) (⊤ , inl refl) (pred∞ n)

IsZero∞ : ℕ∞ → Set
IsZero∞ n = fst (IsZero∞ᵗ n)

IsNotZero∞ᵗ : ℕ∞ → Σ Set (λ A → A ≡ ⊤ ⊎ A ≡ ⊥)
IsNotZero∞ᵗ n = ite-Maybe (λ _ → ⊤ , inl refl) (⊥ , inr refl) (pred∞ n)

IsNotZero∞ : ℕ∞ → Set
IsNotZero∞ n = fst (IsNotZero∞ᵗ n)

instance
  recomputeIsNotZero∞ : {n : ℕ∞} → .⦃ IsNotZero∞ n ⦄ → IsNotZero∞ n
  recomputeIsNotZero∞ {n} with pred∞ n
  ... | just _ = tt

pred∞withProof : (n : ℕ∞) → Σ (Maybe ℕ∞) (ite-Maybe (λ _ → IsNotZero∞ n) (IsZero∞ n))
pred∞withProof n with pred∞ n
... | suc∞ x = suc∞ x , tt
... | zero∞ = zero∞ , tt

ℕ∞→ℕ : ℕ → ℕ∞ → Maybe ℕ
ℕ∞→ℕ zero _ = nothing
ℕ∞→ℕ (suc n) c with pred∞ c
... | zero∞ = just 0
... | suc∞ b with ℕ∞→ℕ n b
... | nothing = nothing
... | just x = just (suc x)

embed : ℕ → ℕ∞
pred∞ (embed zero) = zero∞
pred∞ (embed (suc n)) = suc∞ (embed n)

cover : ℕ → ℕ∞
cover zero = ∞
cover (suc n) = embed n

coite : ∀{i}{B : Set i} → (B → Maybe B) → B → ℕ∞
pred∞ (coite f b) with f b
... | zero∞ = zero∞
... | suc∞ c = suc∞ (coite f c)

succ∞ : ℕ∞ → ℕ∞
pred∞ (succ∞ a) = just a

conat : Maybe ℕ∞ → ℕ∞
pred∞ (conat a) = a

succ∞' : Maybe ℕ∞ → Maybe ℕ∞
succ∞' n = just λ where .pred∞ → n

pred∞' : Maybe ℕ∞ → Maybe ℕ∞
pred∞' nothing = nothing
pred∞' (just x) = pred∞ x

pred∞'' : Maybe ℕ∞ → ℕ∞
pred∞ (pred∞'' nothing) = nothing
pred∞'' (just x) = x

predℕ∞ : (n : ℕ∞) → .⦃ IsNotZero∞ n ⦄ → ℕ∞
predℕ∞ n with pred∞ n
... | suc∞ x = x

unsafe-predℕ∞ : ℕ∞ → ℕ∞
unsafe-predℕ∞ x = pred∞'' (pred∞ x)

add : ℕ∞ → ℕ∞ → ℕ∞
add' : Maybe ℕ∞ → ℕ∞ → Maybe ℕ∞

pred∞ (add x y) = add' (pred∞ x) y
add' zero∞ y = pred∞ y
add' (suc∞ x) y = suc∞ (add x y)

{-
_*_ : ℕ∞ → ℕ∞ → ℕ∞
_*'_ : Maybe ℕ∞ → ℕ∞ → Maybe ℕ∞
suc∞ x *' k = just (k + x * k)
zero∞ *' k = nothing
pred∞ (n * k) = pred∞ n *' k
-}

+-aux : ℕ∞ → ℕ∞ ⊎ ℕ∞ → Maybe (ℕ∞ ⊎ ℕ∞)
+-aux k (inl n) with pred∞ n
... | just n' = just (inl n')
... | nothing with pred∞ k
... | just k' = just (inr k')
... | nothing = nothing
+-aux _ (inr k) with pred∞ k
... | just k' = just (inr k')
... | nothing = nothing

infixl 6 _+_
_+_ : ℕ∞ → ℕ∞ → ℕ∞
_+_ n k = coite (+-aux k) (inl n)

*-aux : ℕ∞ → ℕ∞ × ℕ∞ → Maybe (ℕ∞ × ℕ∞)
*-aux restore (e1 , e2) with pred∞ e1
... | nothing = nothing
... | just e1' with pred∞ e2
... | nothing = nothing
... | just e2' with pred∞ e2'
... | nothing = just (e1' , restore)
... | just e2'' = just (e1 , e2')

infixl 7 _*_
_*_ : ℕ∞ → ℕ∞ → ℕ∞
n * k = coite (*-aux k) (n , k)

^-aux : ℕ → ℕ∞ → ℕ∞ → CoList ℕ∞ → Maybe (ℕ × ℕ∞ × ℕ∞ × CoList ℕ∞)
^-aux zero x₁ x₂ xs₂ with pred∞ x₂
^-aux zero x₁ x₂ xs₂ | nothing = nothing
^-aux zero x₁ x₂ xs₂ | just x₃ = just (1 , x₁ , x₃ , xs₂)
^-aux (suc n) x₁ x₂ xs₂ with pred∞ x₂
^-aux (suc n) x₁ x₂ xs₂ | nothing with uncons xs₂
^-aux (suc n) x₁ x₂ xs₂ | nothing | nothing = nothing
^-aux (suc n) x₁ x₂ xs₂ | nothing | just (x₃ , xs₃) with ^-aux n x₁ x₃ xs₃
^-aux (suc n) x₁ x₂ xs₂ | nothing | just (x₃ , xs₃) | nothing = nothing
^-aux (suc n) x₁ x₂ xs₂ | nothing | just (x₃ , xs₃) | just (n₁ , _ , x₄ , xs₄) = just (suc n₁ , x₁ , x₁ , x₄ ∷ xs₄)
^-aux (suc n) x₁ x₂ xs₂ | just x₃ = just (suc n , x₁ , x₃ , xs₂)

infixr 8 _^_
-- Credits to Szumi for defining exponentiation
_^_ : ℕ∞ → ℕ∞ → ℕ∞
pred∞ (x ^ y) with pred∞ y
pred∞ (x ^ y) | nothing = just (embed 0)
pred∞ (x ^ y) | just y₁ with pred∞ x
pred∞ (x ^ y) | just y₁ | nothing = nothing
pred∞ (x ^ y) | just y₁ | just x₁ = just (coite (λ (n , x₁ , x₂ , xs₂) → ^-aux n x₁ x₂ xs₂) (0 ,' x₁ ,' x₁ ,' coreplicate y₁ x₁))

infix 4 _ℕ≤ℕ∞_

_ℕ≤ℕ∞_ : ℕ → ℕ∞ → Set
zero ℕ≤ℕ∞ k = ⊤
suc n ℕ≤ℕ∞ k with pred∞ k
... | nothing = ⊥
... | just k' = n ℕ≤ℕ∞ k'

infix 4 _ℕ<ℕ∞_

_ℕ<ℕ∞_ : ℕ → ℕ∞ → Set
n ℕ<ℕ∞ k with pred∞ k
n ℕ<ℕ∞ k     | zero∞ = ⊥
zero ℕ<ℕ∞ k  | suc∞ x = ⊤
suc n ℕ<ℕ∞ k | suc∞ x = n ℕ<ℕ∞ x

--------------------------------------------------
-- Older idea of Conat with ⊤ ⊎ _
--------------------------------------------------
{-
conat : ℕ∞' → ℕ∞
pred∞ (conat a) = a

succ∞' : ℕ∞' → ℕ∞'
succ∞' n = inr λ where .pred∞ → n

pred∞' : ℕ∞' → ℕ∞'
pred∞' (inl tt) = inl tt
pred∞' (inr x)  = pred∞ x

pred∞'' : ℕ∞' → ℕ∞
pred∞ (pred∞'' (inl tt)) = inl tt
pred∞'' (inr x) = x

Unwrap-pred : ℕ∞' → Set
Unwrap-pred (inl _) = ⊤
Unwrap-pred (inr _) = ℕ∞

unwrap-pred : (n : ℕ∞') → Unwrap-pred n
unwrap-pred (inl _) = _
unwrap-pred (inr x) = x

infixl 6 _+_ _+′_
_+_ : ℕ∞ → ℕ∞ → ℕ∞
_+′_ : ℕ∞' → ℕ∞ → ℕ∞'

pred∞ (x + y) = pred∞ x +′ y
zero∞ +′ y = pred∞ y
suc∞ x +′ y = suc∞ (x + y)
-}
