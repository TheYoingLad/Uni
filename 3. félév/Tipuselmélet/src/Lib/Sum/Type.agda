{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Type where

open import Agda.Primitive using (_⊔_)

infixr 1 _⊎_
data _⊎_ {i}{j}(A : Set i)(B : Set j) : Set (i ⊔ j) where
  inl : (a : A) → A ⊎ B
  inr : (b : B) → A ⊎ B
