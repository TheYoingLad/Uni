{-# OPTIONS --safe --without-K #-}

module Lib.Containers.List.Properties where

open import Lib.Containers.List.Type
open import Lib.Containers.List.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym
open import Lib.Sigma.Type
open import Lib.Empty.Type
open import Lib.Unit.Type

open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.Nat.Properties

∷-injective : ∀{i}{A : Set i}{x y : A}{xs ys : List A} → x ∷ xs ≡ y ∷ ys → x ≡ y × xs ≡ ys
∷-injective refl = (refl , refl)

∷-injectiveˡ : ∀{i}{A : Set i}{x y : A}{xs ys : List A} → x ∷ xs ≡ y ∷ ys → x ≡ y
∷-injectiveˡ refl = refl

∷-injectiveʳ : ∀{i}{A : Set i}{x y : A}{xs ys : List A} → x ∷ xs ≡ y ∷ ys → xs ≡ ys
∷-injectiveʳ refl = refl

∷-dec : ∀{i}{A : Set i}{x y : A}{xs ys : List A} → Dec (x ≡ y) → Dec (xs ≡ ys) → Dec (x ∷ xs ≡ y ∷ ys)
∷-dec (no p) xs=ys = no λ e → p (∷-injectiveˡ e)
∷-dec (yes p) (no q) = no λ e → q (∷-injectiveʳ e)
∷-dec (yes refl) (yes refl) = yes refl

≡-dec-List : ∀{i}{A : Set i} → ((a b : A) → Dec (a ≡ b)) → (a b : List A) → Dec (a ≡ b)
≡-dec-List _≟_ []       []       = yes refl
≡-dec-List _≟_ (x ∷ xs) []       = no λ ()
≡-dec-List _≟_ []       (y ∷ ys) = no λ ()
≡-dec-List _≟_ (x ∷ xs) (y ∷ ys) = ∷-dec (x ≟ y) (≡-dec-List _≟_ xs ys)

idr++ : ∀{i}{A : Set i}(xs : List A) → xs ++ [] ≡ xs
idr++ [] = refl
idr++ (x ∷ xs) = cong (x ∷_) (idr++ xs)

assoc++ : ∀{i}{A : Set i}(xs ys zs : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
assoc++ [] ys zs = refl
assoc++ (x ∷ xs) ys zs = cong (x ∷_) (assoc++ xs ys zs)

instance
  DecEqList : ∀{i}{A : Set i} → ⦃ DecidableEquality A ⦄ → DecidableEquality (List A)
  DecEqList ⦃ i1 ⦄ = DecProof (≡-dec-List (decide i1))

dist-length++ : ∀{i}{A : Set i}(xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
dist-length++ [] ys = refl
dist-length++ (x ∷ xs) ys = cong suc (dist-length++ xs ys)

reverse-gen : ∀{i}{A : Set i}(xs ys : List A) → foldl (λ rec l → l ∷ rec) ys xs ≡ reverse xs ++ ys
reverse-gen [] ys = refl
reverse-gen (x ∷ xs) ys = reverse-gen xs (x ∷ ys)
                       ◾ sym (assoc++ (reverse xs) (x ∷ []) ys)
                       ◾ cong (_++ ys) (sym (reverse-gen xs (x ∷ [])))

reverse-one : ∀{i}{A : Set i}(x : A)(xs : List A) → reverse (x ∷ xs) ≡ reverse xs ++ x ∷ []
reverse-one x xs = reverse-gen xs (x ∷ [])

reverse-length : ∀{i}{A : Set i}(xs : List A)(n : ℕ) → length xs ≡ n → length (reverse xs) ≡ n
reverse-length [] n e = e
reverse-length (x ∷ xs) (suc n) e = (cong length (reverse-one x xs)
                                 ◾ dist-length++ (reverse xs) (x ∷ [])
                                 ◾ comm+ (length (reverse xs)) 1)
                                 ◾ cong suc (reverse-length xs n (cong pred' e))

dist-reverse++ : ∀{i}{A : Set i}(xs ys : List A) → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
dist-reverse++ [] ys = sym (idr++ (reverse ys))
dist-reverse++ (x ∷ xs) ys = reverse-one x (xs ++ ys)
                          ◾ cong (_++ x ∷ []) (dist-reverse++ xs ys)
                          ◾ assoc++ (reverse ys) (reverse xs) (x ∷ [])
                          ◾ cong (reverse ys ++_) (sym (reverse-one x xs))
