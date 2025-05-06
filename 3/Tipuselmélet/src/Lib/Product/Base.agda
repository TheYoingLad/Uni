{-# OPTIONS --safe --without-K #-}

module Lib.Product.Base where

open import Lib.Product.Type

map : ∀{i j k l}{A : Set i}{B : Set j}{C : Set k}{D : Set l} → 
  (A → C) → (B → D) → A × B → C × D
map f g (a , b) = f a , g b
