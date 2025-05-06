{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Lazy.Base where

open import Lib.Lazy.Type
open import Lib.Nat.Type
open import Lib.Maybe.Type
open import Lib.Sigma.Type
open import Lib.Function.Base

ite : ∀{i j}{A : Set i}{B : Set j} → (A → B) → (Lazy∞ A → B) → Lazy A → B
ite f g (now x) = f x
ite f g (later x) = g x

extract : ∀{i}{A : Set i}(n : ℕ)(da : Lazy A) → Maybe A
extract n       (now a)     = just a
extract zero    (later ∞da) = nothing
extract (suc n) (later ∞da) = extract n (force ∞da)

fmapLazy : ∀{i j}{A : Set i}{B : Set j} → (A → B) → Lazy A → Lazy B
fmapLazy∞ : ∀{i j}{A : Set i}{B : Set j} → (A → B) → Lazy∞ A → Lazy∞ B
fmapLazy f (now x) = now (f x)
fmapLazy f (later x) = later (fmapLazy∞ f x)
force (fmapLazy∞ f a) = fmapLazy f (later a)

liftA2DLazy : ∀{i j k}{A : Set i}{B : Set j}(C : A → B → Set k) → ((a : A) → (b : B) → C a b) → (a : Lazy A)(b : Lazy B) → Lazy (Σ A λ a → Σ B λ b → C a b)
liftA2DLazy∞ : ∀{i j k}{A : Set i}{B : Set j}(C : A → B → Set k) → ((a : A) → (b : B) → C a b) → (a : Lazy∞ A)(b : Lazy∞ B) → Lazy∞ (Σ A λ a → Σ B λ b → C a b)

liftA2DLazy C f (now l) (now r) = now (l , r , f l r)
liftA2DLazy C f (now l) (later r) = later (liftA2DLazy∞ C f (delay (now l)) r)
liftA2DLazy C f (later l) (now r) = later (liftA2DLazy∞ C f l (delay (now r)))
liftA2DLazy C f (later l) (later r) = later (liftA2DLazy∞ C f l r)

force (liftA2DLazy∞ C f l r) = liftA2DLazy C f (force l) (force r)

liftA2Lazy : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A → B → C) → Lazy A → Lazy B → Lazy C
liftA2Lazy∞ : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A → B → C) → Lazy∞ A → Lazy∞ B → Lazy∞ C

liftA2Lazy {C = C} f l r = fmapLazy (snd ∘ snd) (liftA2DLazy (λ _ _ → C) f l r)
liftA2Lazy∞ {C = C} f l r = fmapLazy∞ (snd ∘ snd) (liftA2DLazy∞ (λ _ _ → C) f l r)
