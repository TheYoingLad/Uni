{-# OPTIONS --safe --without-K #-}

module Lib.Containers.HVector.Type where

open import Lib.Nat.Type
open import Lib.Containers.Vector.Type
open import Lib.Level

infixr 5 _∷_
data HVector {i} : {n : ℕ} → Vec (Set i) n → Set (lsuc i) where
  instance [] : HVector []
  _∷_ : ∀{A : Set i}{n}{As : Vec (Set i) n} → A → HVector As → HVector (A ∷ As)
