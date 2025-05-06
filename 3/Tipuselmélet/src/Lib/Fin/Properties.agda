{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Properties where

open import Lib.Fin.Type
open import Lib.Fin.Base
open import Lib.Empty.Type
open import Lib.Empty.Base
open import Lib.Equality
open import Lib.Nat.Type
open import Lib.Dec
open import Lib.Dec.PatternSynonym

¬Fin0 : ¬ Fin 0
¬Fin0 ()

Fin1-η : (a b : Fin 1) → a ≡ b
Fin1-η fzero fzero = refl

fsuc-injective : {n : ℕ}{x y : Fin n} → fsuc x ≡ fsuc y → x ≡ y
fsuc-injective {x = x} e = cong (p x) e where
  p : {k : ℕ} → Fin k → Fin (suc k) → Fin k
  p k fzero = k
  p _ (fsuc x) = x

infix 4 _≟_
_≟_ : ∀{n} → (a b : Fin n) → Dec (a ≡ b)
fzero ≟ fzero = yes refl
fzero ≟ fsuc b = no λ ()
fsuc a ≟ fzero = no λ ()
fsuc a ≟ fsuc b with a ≟ b
... | yes ab = yes (cong fsuc ab)
... | no ¬ab = no λ x → ¬ab (fsuc-injective x)
