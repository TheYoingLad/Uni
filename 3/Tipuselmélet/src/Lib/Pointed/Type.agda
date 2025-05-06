{-# OPTIONS --safe --without-K #-}

module Lib.Pointed.Type where

open import Lib.Level
open import Lib.Sigma.Type

Set∙ : (i : Level) → Set (lsuc i)
Set∙ i = Σ (Set i) (λ A → A)
