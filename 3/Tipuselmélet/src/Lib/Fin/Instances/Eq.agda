{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Instances.Eq where

open import Lib.Fin.Type
open import Lib.Fin.Properties
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Class.Eq

instance
  EqFin : ∀{n} → Eq (Fin n)
  (EqFin Eq.≡ᵗ fzero) fzero = just refl , ⊤ , refl
  (EqFin Eq.≡ᵗ fzero) (fsuc b) = nothing , ⊥ , refl
  (EqFin Eq.≡ᵗ fsuc a) fzero = nothing , ⊥ , refl
  (EqFin Eq.≡ᵗ fsuc a) (fsuc b) with (EqFin Eq.≡ᵗ a) b
  ... | just x , _ , _ = just (cong fsuc x) , ⊤ , refl
  ... | nothing , _ , _ = nothing , ⊥ , refl
  Eq.eqIsJust EqFin {fzero} {fzero} e = tt
  Eq.eqIsJust EqFin {fsuc a} {fsuc b} e with (EqFin Eq.≡ᵗ a) b in eq1
  ... | just x , _ , _ = tt
  ... | nothing , _ , p = 
    let a≡b = fsuc-injective e
        a≡ᵗb = eqIsJust ⦃ EqFin ⦄ a≡b in cast (cong (λ x → IsJust (fst x)) eq1) a≡ᵗb
