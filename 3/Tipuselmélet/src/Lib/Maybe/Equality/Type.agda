{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Equality.Type where

open import Lib.Equality.Type
open import Lib.Maybe.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Level

infix 4 _≡Maybe_

_≡Maybe_ : ∀{i}{A : Set i}(x y : Maybe A) → Set i
nothing ≡Maybe nothing = Lift _ ⊤
nothing ≡Maybe just y = Lift _ ⊥
just x ≡Maybe nothing = Lift _ ⊥
just x ≡Maybe just y = x ≡ y
