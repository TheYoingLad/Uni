{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Properties where

open import Lib.Sum.Type
open import Lib.Sum.Base renaming (swap to swap-⊎)
open import Lib.Equality
open import Lib.Sigma.Type
open import Lib.Dec
open import Lib.Dec.PatternSynonym

inl-injective : ∀{i j}{A : Set i}{B : Set j}{x y : A} → inl {B = B} x ≡ inl y → x ≡ y
inl-injective refl = refl

inr-injective : ∀{i j}{A : Set i}{B : Set j}{x y : B} → inr {A = A} x ≡ inr y → x ≡ y
inr-injective refl = refl

swap-involutive : ∀{i j}{A : Set i}{B : Set j}{x : A ⊎ B} → swap-⊎ (swap-⊎ x) ≡ x
swap-involutive {x = inl a} = refl
swap-involutive {x = inr b} = refl

comm-⊎ : ∀{i j}{A : Set i}{B : Set j} → A ⊎ B ↔ B ⊎ A
comm-⊎ = swap-⊎ , swap-⊎

ass-⊎ : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A ⊎ B) ⊎ C ↔ A ⊎ (B ⊎ C)
ass-⊎ = (λ abc → case abc (λ ab → case ab (λ a → inl a) (λ b → inr (inl b))) (λ c → inr (inr c)))
     , (λ abc → case abc (λ a → inl (inl a)) λ bc → case bc (λ b → inl (inr b)) (λ c → inr c))

dec-⊎ : ∀{i j}{A : Set i}{B : Set j} → ((a b : A) → Dec (a ≡ b)) → ((a b : B) → Dec (a ≡ b)) → (a b : A ⊎ B) → Dec (a ≡ b)
dec-⊎ e1 e2 (inl a) (inl b) = case (e1 a b) (λ e → yes (cong inl e)) (λ e → no λ e3 → e (inl-injective e3))
dec-⊎ e1 e2 (inl a) (inr b) = no λ ()
dec-⊎ e1 e2 (inr a) (inl b) = no λ ()
dec-⊎ e1 e2 (inr a) (inr b) = case (e2 a b) (λ e → yes (cong inr e)) (λ e → no λ e3 → e (inr-injective e3))
