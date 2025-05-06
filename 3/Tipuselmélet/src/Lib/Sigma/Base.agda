{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Base where

open import Lib.Sigma.Type

swap : ∀{i j}{A : Set i}{B : Set j} → A × B → B × A
swap (a , b) = b , a

swap-↔ : ∀{i j}{A : Set i}{B : Set j} → A ↔ B → B ↔ A
swap-↔ = swap

map : ∀{i j k l}{A : Set i}{B : A → Set j}{C : Set k}{D : C → Set l} →
  (A → C) → ({a : A} → B a → {c : C} → D c) → Σ A B → Σ C D
map f g (a , b) = f a , g b

map-× : ∀{i j k l}{A : Set i}{B : Set j}{C : Set k}{D : Set l} →
  (A → C) → (B → D) → A × B → C × D
map-× f g (a , b) = f a , g b

isoΣΠ : ∀{i j k}{A : Set i}{B : A → Set j}{C : (a : A) → B a → Set k} →
  ((ab : Σ A B) → C (fst ab) (snd ab)) ↔ ((a : A) → (b : B a) → C a b)
fst isoΣΠ f a b = f (a , b)
snd isoΣΠ f (a , b) = f a b

curry : ∀{i j k}{A : Set i}{B : A → Set j}{C : (a : A) → B a → Set k} →
  ((ab : Σ A B) → C (fst ab) (snd ab)) → ((a : A) → (b : B a) → C a b)
curry = fst isoΣΠ

uncurry : ∀{i j k}{A : Set i}{B : A → Set j}{C : (a : A) → B a → Set k} →
  ((a : A) → (b : B a) → C a b) → ((ab : Σ A B) → C (fst ab) (snd ab))
uncurry = snd isoΣΠ

iso×→ : ∀{i j k}{A : Set i}{B : Set j}{C : A → B → Set k} →
  ((ab : A × B) → C (fst ab) (snd ab)) ↔ ((a : A) → (b : B) → C a b)
fst iso×→ f a b = f (a , b)
snd iso×→ f (a , b) = f a b

curry' : ∀{i j k}{A : Set i}{B : Set j}{C : A → B → Set k} →
  ((ab : A × B) → C (fst ab) (snd ab)) → ((a : A) → (b : B) → C a b)
curry' = fst iso×→

uncurry' : ∀{i j k}{A : Set i}{B : Set j}{C : A → B → Set k} →
  ((a : A) → (b : B) → C a b) → ((ab : A × B) → C (fst ab) (snd ab))
uncurry' = snd iso×→
