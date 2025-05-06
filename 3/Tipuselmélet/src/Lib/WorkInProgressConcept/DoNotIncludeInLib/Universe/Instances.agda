module Lib.WorkInProgressConcept.DoNotIncludeInLib.Universe.Instances where

open import Lib.WorkInProgressConcept.DoNotIncludeInLib.Universe public
open import Lib.Unit.Type
open import Lib.Unit.Properties
open import Lib.Empty
open import Lib.Conat
open import Lib.Sigma
open import Lib.Bool
open import Lib.Sum
open import Lib.Containers.CoVector.Type
open import Lib.Containers.CoVector.Base

instance

  uni⊥ : Universe ⊥ λ ()
  𝒰surj uni⊥ x = noICF0 x
  𝒰inj uni⊥ _ _ _ = refl

  uni⊤ : Universe ⊤ (λ _ → izero)
  𝒰surj uni⊤ i = tt , (prop1ICF izero i)
  𝒰inj uni⊤ a b _ = ⊤η a b

  uniBool : Universe Bool {2} (if_then izero else isuc izero)
  𝒰surj uniBool izero = true , refl
  𝒰surj uniBool (isuc izero) = false , refl
  𝒰surj uniBool (isuc (isuc i)) = noICF0 i
  𝒰inj uniBool false false x = refl
  𝒰inj uniBool true true x = refl

  uni⊎ : ∀{ℓ κ}{n k : ℕ∞}{A : Set ℓ}{B : Set κ}{𝒰A : A → IndCofin n}{𝒰B : B → IndCofin k} →
         ⦃ uA : Universe A 𝒰A ⦄ →
         ⦃ uB : Universe B 𝒰B ⦄ →
         Universe (A ⊎ B) {n + k} λ x → case x (λ a → {!? (𝒰A a)!}) λ b → {!? (𝒰B b)!}
  uni⊎ = {!!}
{-
  uni⊤ : Universe ⊤ 1
  𝒰 uni⊤ = λ _ → tt
  𝒰surj uni⊤ tt = izero , refl
  𝒰inj uni⊤ i j x = prop1ICF i j
-}
{-
  uniBool : Universe Bool 2
  𝒰 uniBool izero = false
  𝒰 uniBool (isuc _) = true
  𝒰surj uniBool false = izero , refl
  𝒰surj uniBool true = isuc izero , refl
  𝒰inj uniBool izero izero x = refl
  𝒰inj uniBool (isuc izero) (isuc izero) x = refl
  𝒰inj uniBool (isuc izero) (isuc (isuc j)) x = noICF0 j
  𝒰inj uniBool (isuc (isuc i)) (isuc j) x = noICF0 i
-}
