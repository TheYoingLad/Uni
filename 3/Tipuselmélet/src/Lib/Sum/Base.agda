{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Base where

open import Lib.Sum.Type
open import Lib.Equality.Type

case : ∀ {i j k}{A : Set i}{B : Set j}{C : Set k}
         (t : A ⊎ B)(u : A → C)(v : B → C) → C
case (inl t) u v = u t
case (inr t) u v = v t

elim : ∀ {i j k}{A : Set i}{B : Set j}{C : A ⊎ B → Set k}
           (t : A ⊎ B)(u : (a : A) → C (inl a))(v : (b : B) → C (inr b)) → C t
elim (inl t) u v = u t
elim (inr t) u v = v t

ind : ∀ {i j k}{A : Set i}{B : Set j}{C : A ⊎ B → Set k}
          (t : A ⊎ B)
          (u : (a : A) → t ≡ inl a → C (inl a))
          (v : (b : B) → t ≡ inr b → C (inr b)) → C t
ind (inl t) u v = u t refl
ind (inr t) u v = v t refl

fromInr : ∀{i j}{A : Set i}{B : Set j} → (B → A) → A ⊎ B → A
fromInr f t = case t (λ x → x) f

fromInl : ∀{i j}{A : Set i}{B : Set j} → (A → B) → A ⊎ B → B
fromInl f t = case t f (λ x → x)

reduceIdem : ∀{i}{A : Set i} → A ⊎ A → A
reduceIdem t = case t (λ x → x) (λ x → x)

swap : ∀{i j}{A : Set i}{B : Set j} → A ⊎ B → B ⊎ A
swap (inl x) = inr x
swap (inr x) = inl x

map : ∀{i j k l}{A : Set i}{B : Set j}{C : Set k}{D : Set l} →
       (A → C) → (B → D) → (A ⊎ B → C ⊎ D)
map f g t = case t (λ x → inl (f x)) (λ x → inr (g x))

mapᵣ : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} →
       (B → C) → (A ⊎ B → A ⊎ C)
mapᵣ = map (λ x → x)

mapₗ : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} →
       (A → C) → (A ⊎ B → C ⊎ B)
mapₗ f = map f (λ x → x)
