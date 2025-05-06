{-# OPTIONS --safe --without-K #-}

module Lib.Containers.HList.Type where

open import Lib.Containers.List using (List; []; _∷_)
open import Lib.Level

infixr 5 _∷_
data HList {i} : List (Set i) → Set (lsuc i) where
  instance [] : HList []
  _∷_ : ∀{A : Set i}{As : List (Set i)} → A → HList As → HList (A ∷ As)
