{-# OPTIONS --safe --without-K #-}

module Lib.Relation where

open import Lib.Relation.Base public
open import Lib.Equality.Base
open import Lib.Equality.Type
-- open import Lib.Relation.Notation (_≡_) (trans) (refl) public
open import Lib.Relation.Equivalence public
open import Lib.Relation.PartialOrder public
open import Lib.Relation.TotalOrder public
