{-# OPTIONS --safe --without-K #-}

module Lib.Bool.Instances.Eq where

open import Lib.Class.Eq
open import Lib.Bool.Type
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Equality.Type
open import Lib.Unit.Type
open import Lib.Empty.Type

instance
  EqBool : Eq Bool
  (EqBool Eq.≡ᵗ false) false = just refl , ⊤ , refl
  (EqBool Eq.≡ᵗ false) true = nothing , ⊥ , refl
  (EqBool Eq.≡ᵗ true) false = nothing , ⊥ , refl
  (EqBool Eq.≡ᵗ true) true = just refl , ⊤ , refl
  Eq.eqIsJust EqBool {false} {false} e = tt
  Eq.eqIsJust EqBool {true} {true} e = tt
