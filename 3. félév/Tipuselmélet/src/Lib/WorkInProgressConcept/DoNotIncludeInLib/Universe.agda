module Lib.WorkInProgressConcept.DoNotIncludeInLib.Universe where

open import Lib.Conat
open import Lib.Nat
open import Lib.IndCofin public
open import Lib.Sigma
open import Lib.Equality public
open import Lib.Empty public
open import Lib.Containers.CoVector

record Universe {ℓ}(A : Set ℓ){size : ℕ∞}(𝒰 : A → IndCofin size) : Set ℓ where
  field
    𝒰surj : (i : IndCofin size) → Σ A λ a → 𝒰 a ≡ i
    𝒰inj : (a b : A) → 𝒰 a ≡ 𝒰 b → a ≡ b

open Universe public


invert𝒰 : ∀{ℓ}{A : Set ℓ}{n : ℕ∞}{𝒰 : A → IndCofin n} → ⦃ u : Universe A 𝒰 ⦄ → IndCofin n → A
invert𝒰 ⦃ u ⦄ i = fst (𝒰surj u i)

tabulate𝒰 : ∀{ℓ}{A : Set ℓ}{n : ℕ∞}{𝒰 : A → IndCofin n} → ⦃ u : Universe A 𝒰 ⦄ → CoVec A n
tabulate𝒰 = tabulateⁱ invert𝒰
