{-# OPTIONS --safe --without-K #-}

module Lib.Isomorphism.Base where

open import Lib.Isomorphism.Type

cast≅ : ∀{i j}{A : Set i}{B : Set j} → A ≅ B → A → B
cast≅ = coe→
