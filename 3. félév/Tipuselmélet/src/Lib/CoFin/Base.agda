{-# OPTIONS --safe --guardedness --without-K #-}
module Lib.CoFin.Base where

open import Lib.CoFin.Type
open import Lib.CoNat.Type
open import Lib.CoNat.Base using (IsNotZero∞ ; predℕ∞)
open import Lib.CoNat.Literals
open import Lib.Maybe.Type
open import Lib.Sigma.Type
open import Lib.Function
open import Lib.Sum.Type
open import Lib.Empty.Type
open import Lib.Empty.Base
open import Lib.Unit.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Containers.CoVector.Type
open import Lib.Lazy.Type

f∞ : CoFin ∞
fpred∞ f∞ = just f∞

coite : ∀{ℓ}{n : ℕ∞}(P : ℕ∞ → Set ℓ) →
  (ginz : ({n : ℕ∞} → P n → IsNotZero∞ n)) →
  (gfpred∞ : {n : ℕ∞} → (p : P n) → Maybe (P (predℕ∞ n ⦃ ginz p ⦄))) →
  (seed : P n) →
  CoFin n
inz (coite P ginz gfpred∞ seed) = ginz seed
fpred∞ (coite P ginz gfpred∞ seed) with gfpred∞ seed
... | just x = just (coite P ginz gfpred∞ x)
... | nothing = nothing

coiteΣ : ∀{ℓ}{n : ℕ∞}(P : ℕ∞ → Set ℓ) →
  (gen : {n : ℕ∞} → P n → Σ (IsNotZero∞ n) λ p → Maybe (P (predℕ∞ n ⦃ p ⦄))) →
  (seed : P n) →
  CoFin n
coiteΣ P gen seed = coite P (fst ⊚ gen) (snd ⊚ gen) seed

coiteιΣ : ∀{ℓ}{n : ℕ∞}(P : ℕ∞ → Set ℓ) →
  (gen : {n : ℕ∞} → P n → ιΣ (IsNotZero∞ n) (Maybe (P (predℕ∞ n)))) →
  (seed : P n) →
  CoFin n
coiteιΣ P gen seed = coiteΣ P gen seed

singular-cof1 : (c : CoFin 1) → fpred∞ c ≡ nothing
singular-cof1 c with fpred∞ c
... | just x = exfalso (coz x)
... | nothing = refl

cof1 : CoFin 1
fpred∞ cof1 = nothing

cof2-1 : CoFin 2
fpred∞ cof2-1 = nothing

cof2-2 : CoFin 2
fpred∞ cof2-2 = just cof1

diff : cof2-1 ≢ cof2-2
diff x with cong fpred∞ x
... | ()

_‼ᶜ_ : ∀{ℓ}{A : Set ℓ}{n : ℕ∞} → CoVec A n → CoFin n → Lazy A
cs ‼ᶜ f with fpred∞ f
... | just x = later (indx (tail cs ⦃ inz f ⦄) x)
  where
    indx : ∀{ℓ}{A : Set ℓ}{n : ℕ∞} → CoVec A n → CoFin n → Lazy∞ A
    force (indx cs f) = cs ‼ᶜ f
... | nothing = now (head cs ⦃ inz f ⦄)
