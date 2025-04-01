{-# OPTIONS --safe --without-K #-}

module Lib.Ordering.Properties where

open import Lib.Ordering.Type
open import Lib.Dec
open import Lib.Equality.Type
open import Lib.Sum.Type

LT≢GT : LT ≢ GT
LT≢GT ()

GT≢LT : GT ≢ LT
GT≢LT ()

EQ≢GT : EQ ≢ GT
EQ≢GT ()

EQ≢LT : EQ ≢ LT
EQ≢LT ()

GT≢EQ : GT ≢ EQ
GT≢EQ ()

LT≢EQ : LT ≢ EQ
LT≢EQ ()

infix 4 _≟_
_≟_ : (a b : Ordering) → Dec (a ≡ b)
LT ≟ LT = inl refl
LT ≟ EQ = inr (λ ())
LT ≟ GT = inr (λ ())
EQ ≟ LT = inr (λ ())
EQ ≟ EQ = inl refl
EQ ≟ GT = inr (λ ())
GT ≟ LT = inr (λ ())
GT ≟ EQ = inr (λ ())
GT ≟ GT = inl refl
