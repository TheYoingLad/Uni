{-# OPTIONS --safe --without-K #-}

module Lib.Product.Instances.IsTrunc where

open import Lib.Nat.Type
open import Lib.Product.Type
open import Lib.Product.Properties
open import Lib.Sigma.Type renaming (_×_ to _×Σ_; _,_ to _,Σ_)
open import Lib.Class.IsTrunc
open import Lib.Equality.Type
open import Lib.Equality.Base

isTruncType×≡ : ∀{i j}{A : Set i}{B : Set j}{n : ℕ}{x1 x2 : A}{y1 y2 : B} → isTruncType n ((x1 ≡ x2) × (y1 ≡ y2)) → isTruncType n ((x1 , y1) ≡ (x2 , y2))
isTruncType×≡ {n = zero} ((e1 , e2) ,Σ p) = snd ×≡ (e1 , e2) ,Σ λ {refl → cong (snd ×≡) (p (refl , refl))}
isTruncType×≡ {n = suc n} p e1 e2 = {!isTruncType×≡ {n = n}!}

isTruncType× : ∀{i j}{A : Set i}{B : Set j}{n : ℕ} → isTruncType n A → isTruncType n B → isTruncType n (A × B)
isTruncType× {n = zero} (a ,Σ trunc1) (b ,Σ trunc2) = ((a , b) ,Σ λ x → snd ×≡ (trunc1 (fst x) , trunc2 (snd x)))
isTruncType× {n = suc n} isTrunc1 isTrunc2 (x1 , y1) (x2 , y2) = {!isTruncType× {n = n} (isTrunc1 x1 x2) (isTrunc2 y1 y2)!}
