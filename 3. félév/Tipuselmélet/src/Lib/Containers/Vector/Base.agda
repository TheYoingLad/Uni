{-# OPTIONS --safe --without-K #-}

module Lib.Containers.Vector.Base where

open import Lib.Unit.Type
open import Lib.Containers.Vector.Type
open import Lib.Sigma.Type
open import Agda.Builtin.FromNat
open import Lib.Fin.Type
open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.Nat.Literals
open import Lib.Level
open import Lib.Bool.Type
open import Lib.Equality.Type
open import Lib.Equality.Base using (cong)

infixl 6 _[_]%=_
_[_]%=_ : ∀{i}{A : Set i}{n : ℕ} → Vec A n → Fin n → (A → A) → Vec A n
(x ∷ xs) [ fzero ]%= f = f x ∷ xs
(x ∷ xs) [ fsuc i ]%= f = x ∷ xs [ i ]%= f

infixl 6 _[_]≔_
_[_]≔_ : ∀{i}{A : Set i}{n : ℕ} → Vec A n → Fin n → A → Vec A n
xs [ i ]≔ y = xs [ i ]%= (λ _ → y)

cast : ∀{i}{A : Set i}{m n : ℕ} → .(eq : m ≡ n) → Vec A m → Vec A n
cast {n = zero}  eq []       = []
cast {n = suc _} eq (x ∷ xs) = x ∷ cast (cong pred' eq) xs

map : ∀{i j}{A : Set i}{B : Set j}{n : ℕ} → (A → B) → Vec A n → Vec B n
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

infixr 5 _++_
_++_ : ∀{i}{A : Set i}{m n : ℕ} → Vec A m → Vec A n → Vec A (m + n)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

concat : ∀{i}{A : Set i}{m n : ℕ} → Vec (Vec A m) n → Vec A (n * m)
concat []         = []
concat (xs ∷ xss) = xs ++ concat xss

lengthᵗ : ∀{i}{A : Set i}{n : ℕ} → Vec A n → Σ ℕ (n ≡_)
lengthᵗ {n = .0} [] = 0 , refl
lengthᵗ {n = .(suc _)} (x ∷ xs) = let (n , p) = lengthᵗ xs in suc n , cong suc p

length : ∀{i}{A : Set i}{n : ℕ} → Vec A n → ℕ
length xs = fst (lengthᵗ xs)

head : ∀{i}{A : Set i}{n : ℕ} → Vec A (suc n) → A
head (x ∷ xs) = x

tail : ∀{i}{A : Set i}{n : ℕ} → Vec A (suc n) → Vec A n
tail (x ∷ xs) = xs

last : ∀{i}{A : Set i}{n : ℕ} → Vec A (suc n) → A
last (x ∷ []) = x
last (_ ∷ xs@(_ ∷ _)) = last xs

init : ∀{i}{A : Set i}{n : ℕ} → Vec A (suc n) → Vec A n
init (_ ∷ []) = []
init (x ∷ xs@(_ ∷ _)) = x ∷ init xs

infixl 10 _‼_
_‼_ : ∀{i}{A : Set i}{n : ℕ} → Vec A n → Fin n → A
_‼_ (x ∷ xs) fzero    = x
_‼_ (x ∷ xs) (fsuc i) = _‼_ xs i

iterateVec : ∀{i}{A : Set i} → (A → A) → A → ∀ {n} → Vec A n
iterateVec s z {zero}  = []
iterateVec s z {suc n} = z ∷ iterateVec s (s z)

replicate : ∀{i}{A : Set i}(n : ℕ) → A → Vec A n
replicate zero a = []
replicate (suc n) a = a ∷ replicate n a

foldr : ∀{i j}{A : Set i}(B : ℕ → Set j){n : ℕ} → (∀{n} → A → B n → B (suc n)) → B zero → Vec A n → B n
foldr B _⊕_ e []       = e
foldr B _⊕_ e (x ∷ xs) = x ⊕ foldr B _⊕_ e xs

foldl : ∀{i j}{A : Set i}(B : ℕ → Set j){n : ℕ} → (∀ {n} → B n → A → B (suc n)) → B zero → Vec A n → B n
foldl B _⊕_ e []       = e
foldl B _⊕_ e (x ∷ xs) = foldl (λ x → B (suc x)) _⊕_ (e ⊕ x) xs

foldr′ : ∀{i j}{A : Set i}{B : Set j}{n : ℕ} → (A → B → B) → B → Vec A n → B
foldr′ _⊕_ = foldr _ _⊕_

foldl′ : ∀{i j}{A : Set i}{B : Set j}{n : ℕ} → (B → A → B) → B → Vec A n → B
foldl′ _⊕_ = foldl _ _⊕_

foldr₁ : ∀{i}{A : Set i}{n : ℕ} → (A → A → A) → Vec A (suc n) → A
foldr₁ _⊕_ (x ∷ [])     = x
foldr₁ _⊕_ (x ∷ y ∷ ys) = x ⊕ foldr₁ _⊕_ (y ∷ ys)

foldl₁ : ∀{i}{A : Set i}{n : ℕ} → (A → A → A) → Vec A (suc n) → A
foldl₁ _⊕_ (x ∷ xs) = foldl _ _⊕_ x xs

indr-Vec : ∀{i j}{A : Set i}(B : ℕ → Set j){n : ℕ} →
  ({k n : ℕ} → k ≡ suc n → A → B n → B (suc n)) → ({k : ℕ} → k ≡ zero → B zero) → Vec A n → B n
indr-Vec B f e [] = e refl
indr-Vec B f e (x ∷ xs) = f refl x (indr-Vec B f e xs)

indl-Vec : ∀{i j}{A : Set i}(B : ℕ → Set j){n : ℕ} →
  ({k n : ℕ} → k ≡ suc n → B n → A → B (suc n)) → ({k : ℕ} → k ≡ zero → B zero) → Vec A n → B n
indl-Vec B f e [] = e refl
indl-Vec B f e (x ∷ xs) = indl-Vec (λ k → B (suc k)) (λ where refl → f refl) (λ {refl → f {1} {0} refl (e refl) x}) xs

sumℕ : {n : ℕ} → Vec ℕ n → ℕ
sumℕ = foldr _ _+_ 0

splitAt : ∀{i}{A : Set i} → ∀ m {n} (xs : Vec A (m + n)) →
          Σ (Vec A m) (λ ys → Σ (Vec A n) (λ zs → xs ≡ ys ++ zs))
splitAt zero    xs                = ([] , xs , refl)
splitAt (suc m) (x ∷ xs)          with splitAt m xs
splitAt (suc m) (x ∷ .(ys ++ zs)) | (ys , zs , refl) =
  ((x ∷ ys) , zs , refl)

take : ∀{i}{A : Set i} → ∀ m {n} → Vec A (m + n) → Vec A m
take m xs          with splitAt m xs
take m .(ys ++ zs) | (ys , zs , refl) = ys

drop : ∀{i}{A : Set i} → ∀ m {n} → Vec A (m + n) → Vec A n
drop m xs          with splitAt m xs
drop m .(ys ++ zs) | (ys , zs , refl) = zs
