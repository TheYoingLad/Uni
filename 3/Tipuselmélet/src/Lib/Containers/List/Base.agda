{-# OPTIONS --safe --without-K #-}

module Lib.Containers.List.Base where

open import Lib.Containers.List.Type
open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym
open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.Nat.Literals
open import Lib.CoNat.Type
open import Lib.CoNat.Base renaming (_+_ to _+∞_; _*_ to _*∞_; _^_ to _^∞_)
open import Lib.CoNat.Literals
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Bool.Type
open import Lib.Bool.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Fin.Type
open import Lib.Maybe.Type
open import Lib.Sum.Type
open import Lib.Sigma.Type
open import Lib.UnitOrEmpty.Type

length : ∀{i}{A : Set i} → List A → ℕ
length [] = 0
length (x ∷ xs) = suc (length xs)

sumℕ : List ℕ → ℕ
sumℕ [] = 0
sumℕ (x ∷ xs) = x + sumℕ xs

productℕ : List ℕ → ℕ
productℕ [] = 1
productℕ (x ∷ xs) = x * productℕ xs

sumℕ∞ : List ℕ∞ → ℕ∞
sumℕ∞ [] = 0
sumℕ∞ (x ∷ xs) = x +∞ sumℕ∞ xs

productℕ∞ : List ℕ∞ → ℕ∞
productℕ∞ [] = 1
productℕ∞ (x ∷ xs) = x *∞ productℕ∞ xs

infixr 5 _++_
_++_ : ∀{i}{A : Set i} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys

map : ∀{i j}{A : Set i}{B : Set j} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

ite-List : ∀{i j}{A : Set i}{B : Set j} → B → (A → B → B) → List A → B
ite-List n f [] = n
ite-List n f (x ∷ xs) = f x (ite-List n f xs)

foldr : ∀{i j}{A : Set i}{B : Set j} → (A → B → B) → B → List A → B
foldr f n = ite-List n f

foldl : ∀{i j}{A : Set i}{B : Set j} → (B → A → B) → B → List A → B
foldl f n []       = n
foldl f n (x ∷ xs) = foldl f (f n x) xs

filter : ∀{i j}{A : Set i}{P : A → Set j} → Decidable P → List A → List A
filter p [] = []
filter p (x ∷ xs) with decide p x
... | yes f = x ∷ filter p xs
... | no f = filter p xs

filterᵇ : ∀{i}{A : Set i} → (A → Bool) → List A → List A
filterᵇ p [] = []
filterᵇ p (x ∷ xs) = let r = filterᵇ p xs in if p x then x ∷ r else r

reverseNaive : ∀{i}{A : Set i} → List A → List A
reverseNaive [] = []
reverseNaive (x ∷ xs) = reverseNaive xs ++ x ∷ []

reverse : ∀{i}{A : Set i} → List A → List A
reverse = foldl (λ acc b → b ∷ acc) []

intersperse : ∀{i}{A : Set i} → A → List A → List A
intersperse x []       = []
intersperse x (y ∷ []) = y ∷ []
intersperse x (y ∷ ys@(_ ∷ _)) = y ∷ x ∷ intersperse x ys

intercalate : ∀{i}{A : Set i} → List A → List (List A) → List A
intercalate xs []         = []
intercalate xs (ys ∷ [])  = ys
intercalate xs (ys ∷ yss@(_ ∷ _)) = ys ++ xs ++ intercalate xs yss

zipWith : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A → B → C) → List A → List B → List C
zipWith f []       _        = []
zipWith f (_ ∷ _)  []       = []
zipWith f (x ∷ xs) (y ∷ ys) = f x y ∷ zipWith f xs ys

concat : ∀{i}{A : Set i} → List (List A) → List A
concat = foldr _++_ []

null : ∀{i}{A : Set i} → List A → Bool
null []       = true
null (x ∷ xs) = false

Nullᵗ : ∀{i}{A : Set i} → List A → ⊤or⊥
Nullᵗ []       = ⊤ , inl refl
Nullᵗ (_ ∷ _) = ⊥ , inr refl

Null : ∀{i}{A : Set i} → List A → Set
Null xs = fst (Nullᵗ xs)

NotNullᵗ : ∀{i}{A : Set i} → List A → ⊤or⊥
NotNullᵗ []       = ⊥ , inr refl
NotNullᵗ (_ ∷ _) = ⊤ , inl refl

NotNull : ∀{i}{A : Set i} → List A → Set
NotNull xs = fst (NotNullᵗ xs)

replicate : ∀{i}{A : Set i} → ℕ → A → List A
replicate zero    x = []
replicate (suc n) x = x ∷ replicate n x

infixl 10 _‼_
_‼_ : ∀{i}{A : Set i}(xs : List A) → Fin (length xs) → A
(x ∷ _) ‼ fzero    = x
(_ ∷ xs) ‼ (fsuc i) = xs ‼ i

head' : ∀{i}{A : Set i} → List A → Maybe A
head' []      = nothing
head' (x ∷ _) = just x

tail' : ∀{i}{A : Set i} → List A → Maybe (List A)
tail' []       = nothing
tail' (_ ∷ xs) = just xs

last' : ∀{i}{A : Set i} → List A → Maybe A
last' []       = nothing
last' (x ∷ []) = just x
last' (_ ∷ xs@(_ ∷ _)) = last' xs

init'  : ∀{i}{A : Set i} → List A → Maybe (List A)
init' []       = nothing
init' (_ ∷ []) = just []
init' (x ∷ xs@(_ ∷ _)) with init' xs
... | just ys = just (x ∷ ys)
... | nothing = nothing

head : ∀{i}{A : Set i} → (xs : List A) → .⦃ NotNull xs ⦄ → A
head (x ∷ _) = x

tail : ∀{i}{A : Set i} → (xs : List A) → .⦃ NotNull xs ⦄ → List A
tail (_ ∷ xs) = xs

last : ∀{i}{A : Set i} → (xs : List A) → .⦃ NotNull xs ⦄ → A
last (x ∷ []) = x
last (_ ∷ xs@(_ ∷ _)) = last xs

init  : ∀{i}{A : Set i} → (xs : List A) → .⦃ NotNull xs ⦄ → List A
init (_ ∷ []) = []
init (x ∷ xs@(_ ∷ _)) = x ∷ init xs

take : ∀{i}{A : Set i} → ℕ → List A → List A
take zero    xs       = []
take (suc n) []       = []
take (suc n) (x ∷ xs) = x ∷ take n xs

drop : ∀{i}{A : Set i} → ℕ → List A → List A
drop zero    xs       = xs
drop (suc n) []       = []
drop (suc n) (x ∷ xs) = drop n xs

splitAt : ∀{i}{A : Set i} → ℕ → List A → List A × List A
splitAt zero    xs       = ([] , xs)
splitAt (suc n) []       = ([] , [])
splitAt (suc n) (x ∷ xs) = let (a , b) = splitAt n xs in (x ∷ a , b)

digits : ℕ → List ℕ
digits 0 = 0 ∷ []
digits n@(suc _) = digitsWithFuel n n [] where
  digitsWithFuel : ℕ → ℕ → List ℕ → List ℕ
  digitsWithFuel fuel zero acc = acc
  digitsWithFuel zero n@(suc _) acc = []
  digitsWithFuel (suc fuel) n@(suc _) acc = digitsWithFuel fuel (n div 9) (n mod 9 ∷ acc)
