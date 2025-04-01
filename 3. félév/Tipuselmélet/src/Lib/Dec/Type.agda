{-# OPTIONS --safe --without-K #-}

module Lib.Dec.Type where

open import Lib.Level
open import Lib.Bool.Type
open import Lib.Equality.Type
open import Lib.Sum.Type
open import Lib.Empty.Type
open import Lib.Unit.Type

{-
infix 2 _because_
record Dec {p} (P : Set p) : Set p where
  constructor _because_
  field
    does  : Bool
    proof : Reflects P does
  
  pattern yes p = record { does = true ; proof = ofʸ p }
  pattern no ¬p = record { does = false ; proof = ofⁿ ¬p }

open Dec public
-}

Dec : ∀{i}(A : Set i) → Set i
Dec A = A ⊎ (A → ⊥)

record Decidable {i}{j}{A : Set i}(P : A → Set j) : Set (i ⊔ j) where
  constructor DecProof
  field
    decide : ∀ x → Dec (P x)

open Decidable {{...}} public

record Decidable₂ {i}{j}{k}{A : Set i}{B : A → Set j}(P : (a : A) → B a → Set k) : Set (i ⊔ j ⊔ k) where
  constructor DecProof
  field
    decide : ∀ x y → Dec (P x y)

open Decidable₂ {{...}} public

DecidableEquality : ∀{i}(A : Set i) → Set _
DecidableEquality A = Decidable₂ {A = A} _≡_
