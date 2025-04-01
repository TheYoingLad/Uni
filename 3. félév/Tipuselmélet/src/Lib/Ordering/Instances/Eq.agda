{-# OPTIONS --safe --without-K #-}

module Lib.Ordering.Instances.Eq where

open import Lib.Class.Eq
open import Lib.Ordering.Type
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Equality.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Class.Eq

instance
  EqOrdering : Eq Ordering
  (EqOrdering Eq.≡ᵗ LT) LT = just refl , ⊤ , refl
  (EqOrdering Eq.≡ᵗ LT) EQ = nothing , ⊥ , refl
  (EqOrdering Eq.≡ᵗ LT) GT = nothing , ⊥ , refl
  (EqOrdering Eq.≡ᵗ EQ) LT = nothing , ⊥ , refl
  (EqOrdering Eq.≡ᵗ EQ) EQ = just refl , ⊤ , refl
  (EqOrdering Eq.≡ᵗ EQ) GT = nothing , ⊥ , refl
  (EqOrdering Eq.≡ᵗ GT) LT = nothing , ⊥ , refl
  (EqOrdering Eq.≡ᵗ GT) EQ = nothing , ⊥ , refl
  (EqOrdering Eq.≡ᵗ GT) GT = just refl , ⊤ , refl
  Eq.eqIsJust EqOrdering {LT} {LT} e = tt
  Eq.eqIsJust EqOrdering {EQ} {EQ} e = tt
  Eq.eqIsJust EqOrdering {GT} {GT} e = tt
