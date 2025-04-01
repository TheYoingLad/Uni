{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Type where

open import Lib.Irrelevant public

private
  data Empty : Set where

⊥ : Set
⊥ = Irrelevant Empty

{-# DISPLAY Irrelevant Empty = ⊥ #-}
