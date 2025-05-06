{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Instances.IsSet where

open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Maybe.Properties
open import Lib.Maybe.Equality
open import Lib.Class.IsSet
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties
open import Lib.Sigma.Type

instance
  IsSetMaybe : ∀{i}{A : Set i} → ⦃ IsSet A ⦄ → IsSet (Maybe A)
  IsSetMaybe {A = A} ⦃ isSet-proof isSetA ⦄ = isSet'→IsSet λ { nothing nothing refl refl → refl ; (just x) .(just x) refl q → cong (cong just) (fst (isSetA x x refl (just-injective q))) ◾ ≡→≡Maybe→≡-id (just x) (just x) q}


------------ Ez nem ide való, egyelőre még nincs jobb helye !!!

open import Lib.Class.IsProp

IsPropMaybe : ∀{i}{A : Set i} → IsProp (Maybe A) → IsProp A
IsPropMaybe (isProp-proof isPropA) = isProp-proof λ x y → let (p1 , p2) = isPropA (just x) (just y) in just-injective p1 , λ {refl → cong just-injective (p2 refl)}

open import Lib.Empty.Type
open import Lib.Empty.Base
bot : ∀{i}{A : Set i} → IsProp (Maybe A) → Σ (A → ⊥) (λ f → Σ (⊥ → A) λ g → (∀ x → f (g x) ≡ x) × (∀ x → g (f x) ≡ x))
bot (isProp-proof isPropA) = (λ a → just-nothing-disjunctive (fst (isPropA (just a) nothing))) , exfalso , (λ ()) , λ a → let (p1 , p2) = isPropA (just a) nothing in exfalso (just-nothing-disjunctive p1)
