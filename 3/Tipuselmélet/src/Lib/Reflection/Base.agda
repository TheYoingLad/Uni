{-# OPTIONS --safe --without-K #-}

module Lib.Reflection.Base where

open import Lib.Unit.Type
open import Lib.Function
open import Lib.Containers.List.Type
open import Lib.Containers.List.Base
open import Lib.Bool.Type
open import Lib.Bool.Base
open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.CoNat.Type
open import Lib.CoNat.Base renaming (_+_ to _+∞_; _*_ to _*∞_; _^_ to _^∞_)
open import Lib.CoNat.Literals

open import Agda.Builtin.Reflection public
open import Agda.Builtin.String public

fmapTC : ∀{i j}{A : Set i}{B : Set j} → (A → B) → TC A → TC B
fmapTC f tc = bindTC tc (λ a → returnTC (f a))

apTC : ∀{i j}{A : Set i}{B : Set j} → TC (A → B) → TC A → TC B
apTC tcF tcA = bindTC tcF (λ f → bindTC tcA (λ a → returnTC (f a)))

liftA2TC : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A → B → C) → TC A → TC B → TC C
liftA2TC f tcA tcB = apTC (fmapTC f tcA) tcB

constBindTC : ∀{i j}{A : Set i}{B : Set j} → TC A → TC B → TC B
constBindTC tcA tcB = bindTC tcA (λ _ → tcB)

concatString : List String → String
concatString [] = ""
concatString (s ∷ ss) = primStringAppend s (concatString ss)

showNat : ℕ → String
showNat n = showNatFuel n n where
  showNatFuel : ℕ → ℕ → String
  showNatFuel fuel 0 = "0"
  showNatFuel fuel 1 = "1"
  showNatFuel fuel 2 = "2"
  showNatFuel fuel 3 = "3"
  showNatFuel fuel 4 = "4"
  showNatFuel fuel 5 = "5"
  showNatFuel fuel 6 = "6"
  showNatFuel fuel 7 = "7"
  showNatFuel fuel 8 = "8"
  showNatFuel fuel 9 = "9"
  showNatFuel zero (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc _)))))))))) = "No more fuel!"
  showNatFuel (suc fuel) n@(suc (suc (suc (suc (suc (suc (suc (suc (suc (suc _)))))))))) = concatString (map (showNatFuel fuel) (digits n))

showTerm : Term → String
showTerm (var x args) = "var"
showTerm (con c args) = "con"
showTerm (def f args) = primShowQName f
showTerm (lam v t) = "lam"
showTerm (pat-lam cs args) = "pat-lam"
showTerm (pi a b) = "pi"
showTerm (agda-sort s) = "agda-sort"
showTerm (lit l) = "lit"
showTerm (meta x x₁) = "meta"
showTerm unknown = "unknown"

macro
  doesNotTypeCheck : Term → Term → String → Term → TC ⊤
  doesNotTypeCheck t₁ t₂ msg hole = let info = arg-info visible (modality relevant quantity-0) in
    bindTC
      (catchTC (bindTC (inferType (def (quote _$_) (arg info t₁ ∷ arg info t₂ ∷ []))) (λ _ → returnTC true)) (returnTC false)) (λ b →
      if b then typeError (strErr msg ∷ []) else unify hole (quoteTerm ⊤))
{-
    where
      metaMagic : String → Term → TC ⊤
      metaMagic msg (var x args) = typeError (strErr "var" ∷ [])
      metaMagic msg (con c args) = typeError (strErr "con" ∷ [])
      metaMagic msg (def f args) = typeError (termErr t₁ ∷ strErr " " ∷ termErr t₂ ∷ strErr " " ∷ termErr hole ∷ [])
      metaMagic msg (lam v t) = typeError (strErr "lam" ∷ [])
      metaMagic msg (pat-lam cs args) = typeError (strErr "pat-lam" ∷ [])
      metaMagic msg (pi a b) = typeError (strErr "pi" ∷ [])
      metaMagic msg (agda-sort s) = typeError (strErr "agda-sort" ∷ [])
      metaMagic msg (lit l) = typeError (strErr "lit" ∷ [])
      metaMagic msg (meta m _) = blockTC (blockerMeta m)
      metaMagic msg unknown = typeError (strErr "Ide jutottunk?" ∷ [])
-}
-- if b then metaMagic msg hole else unify hole (quoteTerm ⊤))

{-
∣_∣T : Type → TC ℕ∞
∣ t ∣T = ?
-}
