{-# OPTIONS --safe --without-K #-}

module Lib.Containers.Vector.Properties where

open import Lib.Containers.Vector.Type
open import Lib.Containers.Vector.Base
open import Lib.Sigma.Type
open import Lib.Equality -- all 3 modules needed
open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym
open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.Nat.Properties
open import Lib.Unit.Type
open import Lib.Empty.Type

∷-injectiveˡ : ∀{i}{A : Set i}{n : ℕ}{x y : A}{xs ys : Vec A n} → 
  x ∷ xs ≡ y ∷ ys → x ≡ y
∷-injectiveˡ refl = refl

∷-injectiveʳ : ∀{i}{A : Set i}{n : ℕ}{x y : A}{xs ys : Vec A n} →
  x ∷ xs ≡ y ∷ ys → xs ≡ ys
∷-injectiveʳ refl = refl

∷-injective : ∀{i}{A : Set i}{n : ℕ}{x y : A}{xs ys : Vec A n} → 
  (x ∷ xs) ≡ (y ∷ ys) → x ≡ y × xs ≡ ys
∷-injective refl = refl , refl

∷-dec : ∀{i}{A : Set i}{n : ℕ}{x y : A}{xs ys : Vec A n} → Dec (x ≡ y) → Dec (xs ≡ ys) → Dec (x ∷ xs ≡ y ∷ ys)
∷-dec (no p) _ = no λ e → p (∷-injectiveˡ e)
∷-dec (yes p) (no q) = no λ e → q (∷-injectiveʳ e)
∷-dec (yes refl) (yes refl) = yes refl

≡-dec-Vec : ∀{i}{A : Set i}{n : ℕ} → ((a b : A) → Dec (a ≡ b)) → (a b : Vec A n) → Dec (a ≡ b)
≡-dec-Vec _≟_ []       []       = yes refl
≡-dec-Vec _≟_ (x ∷ xs) (y ∷ ys) = ∷-dec (x ≟ y) (≡-dec-Vec _≟_ xs ys)

instance
  DecEqVec : ∀{i}{A : Set i}{n} → ⦃ DecidableEquality A ⦄ → DecidableEquality (Vec A n)
  DecEqVec ⦃ i1 ⦄ = DecProof (≡-dec-Vec (decide i1))

substVec-suc : ∀{i}{A : Set i}{n}(x : A)(xs : Vec A n){m}(e : n ≡ m) → subst (λ k → Vec A (suc k)) e (x ∷ xs) ≡ x ∷ subst (Vec A) e xs
substVec-suc {n = n} x xs {.n} refl = refl
