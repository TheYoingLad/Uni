{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Type where

data Maybe {a} (A : Set a) : Set a where
  nothing : Maybe A
  just : A → Maybe A

infixl 20 _？
_？ : ∀{i}(A : Set i) → Set i
_？ = Maybe
