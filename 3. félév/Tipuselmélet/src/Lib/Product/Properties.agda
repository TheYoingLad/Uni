{-# OPTIONS --safe --without-K #-}

module Lib.Product.Properties where

open import Lib.Product.Type
open import Lib.Product.Base
open import Lib.Equality
open import Lib.Dec
open import Lib.Sum.Type
open import Lib.Sum.Base
open import Lib.Dec.PatternSynonym

×β₁ : ∀{i j}{A : Set i}{B : Set j}{a : A}{b : B} → fst (a , b) ≡ a
×β₁ = refl

×β₂ : ∀{i j}{A : Set i}{B : Set j}{a : A}{b : B} → snd (a , b) ≡ b
×β₂ = refl

×η : ∀{i j}{A : Set i}{B : Set j}{ab : A × B} → (fst ab , snd ab) ≡ ab
×η = refl

×≡ : ∀{i j}{A : Set i}{B : Set j}{a1 a2 : A}{b1 b2 : B} →
  (a1 , b1) ≡ (a2 , b2) ↔ a1 ≡ a2 × b1 ≡ b2
×≡ = to , from where
  to : ∀{i j}{A : Set i}{B : Set j}{a1 a2 : A}{b1 b2 : B} →
    (a1 , b1) ≡ (a2 , b2) → a1 ≡ a2 × b1 ≡ b2
  to e1 = cong fst e1 , cong snd e1

  from : ∀{i j}{A : Set i}{B : Set j}{a1 a2 : A}{b1 b2 : B} →
    a1 ≡ a2 × b1 ≡ b2 → (a1 , b1) ≡ (a2 , b2)
  from (e1 , e2) = cong₂ _,_ e1 e2

dec× : ∀{i j}{A : Set i}{B : Set j} → Dec A → Dec B → Dec (A × B)
dec× (no ¬p) _       = no λ ab → ¬p (fst ab)
dec× (yes p) (yes q) = yes (p , q)
dec× (yes p) (no ¬q) = no λ ab → ¬q (snd ab)
