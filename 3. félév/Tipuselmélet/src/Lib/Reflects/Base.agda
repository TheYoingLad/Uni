{-# OPTIONS --safe --without-K #-}

module Lib.Reflects.Base where

open import Lib.Bool.Type
open import Lib.Bool.Base
open import Lib.Empty.Base

data Reflects {p} (P : Set p) : Bool → Set p where
  ofʸ : ( p :   P) → Reflects P true
  ofⁿ : (¬p : ¬ P) → Reflects P false

of : ∀{i}{P : Set i}{b : Bool} → if b then P else ¬ P → Reflects P b
of {b = true }  p = ofʸ p
of {b = false} ¬p = ofⁿ ¬p

invert : ∀{i}{P : Set i}{b : Bool} → Reflects P b → if b then P else ¬ P
invert (ofʸ  p) = p
invert (ofⁿ ¬p) = ¬p
