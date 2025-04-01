{-# OPTIONS --safe --without-K #-}

module Lib.Dec.InstanceGenerators.Eq where

open import Lib.Dec.Type
open import Lib.Dec.Base

open import Lib.Class.Eq

open import Lib.Sigma.Type

open import Lib.Sum.Type
open import Lib.Sum.Base renaming ( elim to elim-⊎ )

open import Lib.Maybe.Type

open import Lib.Equality.Type

open import Lib.Unit.Type

open import Lib.Empty.Type
open import Lib.Empty.Base

DecidableEquality→Eq : ∀{i}{A : Set i} → DecidableEquality A → Eq A
Eq._≡ᵗ_ (DecidableEquality→Eq inst) a b = elim-⊎ (decide inst a b) (λ e → just e , ⊤ , refl) (λ e → nothing , ⊥ , refl)
Eq.eqIsJust (DecidableEquality→Eq inst) {a} {b} e with decide inst a b
... | inl e2 = tt
... | inr e2 = e2 e
