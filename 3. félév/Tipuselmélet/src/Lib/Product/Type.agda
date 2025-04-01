{-# OPTIONS --safe --without-K #-}

module Lib.Product.Type where

open import Agda.Primitive
  using (_⊔_)

infixr 2 _×_
infixr 4 _,_
record _×_ {a} {b} (A : Set a) (B : Set b) : Set (a ⊔ b) where
  inductive
  constructor _,_
  field
    fst : A
    snd : B

open _×_ public

infix 0 _↔_
_↔_ : ∀{i j} → Set i → Set j → Set (i ⊔ j)
A ↔ B = (A → B) × (B → A)
