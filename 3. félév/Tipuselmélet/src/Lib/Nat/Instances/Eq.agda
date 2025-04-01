{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Instances.Eq where

open import Lib.Class.Eq
open import Lib.Nat.Type
open import Lib.Nat.Properties
open import Lib.Sigma.Type
open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Unit.Type
open import Lib.Empty.Type

_≡ᵗℕ_ : (a b : ℕ) → Σ (Maybe (a ≡ b)) (λ x → Σ Set (IsJust x ≡_))
zero ≡ᵗℕ zero = just refl , ⊤ , refl
zero ≡ᵗℕ suc b = nothing , ⊥ , refl
suc a ≡ᵗℕ zero = nothing , ⊥ , refl
suc a ≡ᵗℕ suc b with a ≡ᵗℕ b
... | nothing , _ = nothing , ⊥ , refl
... | just x , _ = just (cong suc x) , ⊤ , refl

eqIsJustℕ : {a b : ℕ} → .(a ≡ b) → IsJust (fst (a ≡ᵗℕ b))
eqIsJustℕ {zero} {zero} e = tt
eqIsJustℕ {suc a} {suc b} e with a ≡ᵗℕ b in eq1
... | nothing , _ = subst (λ x → IsJust (fst x)) eq1 (eqIsJustℕ {a} {b} (suc-injective e))
... | just x , _ = tt

instance
  Eqℕ : Eq ℕ
  Eqℕ = EqInstance _≡ᵗℕ_ λ e → eqIsJustℕ e
  --                     ^^^^^^^^^^^^^^^^^
  --                     NOT η-expanded, e is relevant in lambda, irrelevant as parameter to eqIsJustℕ.
