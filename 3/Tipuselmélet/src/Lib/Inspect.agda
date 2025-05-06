{-# OPTIONS --safe --without-K #-}

module Lib.Inspect where

open import Agda.Primitive
open import Lib.Equality.Type

infix 50 Reveal_∙_is_
record Reveal_∙_is_ {i j}{A : Set i}{B : A → Set j}(f : (x : A) → B x)(x : A)(y : B x) : Set (i ⊔ j) where
  constructor reveal
  field
    equality : f x ≡ y

open Reveal_∙_is_ public

inspect : ∀{i j}{A : Set i}{B : A → Set j}(f : (x : A) → B x)(x : A) → Reveal f ∙ x is f x
inspect f x = reveal refl

{-
infix 50 Reveal_is_
record Reveal_is_ {i}{A : Set i}.(x : A)(y : A) : Set i where
  constructor reveal'
  field
    equality' : x ≡ y

open Reveal_is_ public

inspect' : ∀{i}{A : Set i}(x : A) → Reveal x is x
inspect' x = reveal' refl
-}
