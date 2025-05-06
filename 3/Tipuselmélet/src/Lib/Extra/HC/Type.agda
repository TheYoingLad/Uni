{-# OPTIONS --safe #-}
module Lib.Extra.HC.Type where


open import Lib using (_≡_; refl; lsuc; _∘_; id)
open import Lib.Extra.Partial.Type
open import Lib.Extra.Semigroupoid.Type
open import Lib.Extra.Category.Type
open import Lib.Extra.Class

instance HCp : ∀{i} → Partial {adj = lsuc} ((Set i) → (Set i)) (λ x → x ≡ (Set i → Set i)) _∘_
HCp = record {}

instance HCa : ∀{i} → Assoc {lsuc i} (Set i → Set i) _∘_
Assoc.ass∘ HCa a b c = refl

instance HCs : ∀{i} → Semigroupoid {adj = lsuc} ((Set i) → (Set i)) (λ x → x ≡ (Set i → Set i)) _∘_
HCs = record {}

instance HCil : ∀{i} → LeftIdent {lsuc i} (Set i → Set i) _∘_ id
LeftIdent.idl HCil a = refl

instance HCir : ∀{i} → RightIdent {lsuc i} (Set i → Set i) _∘_ id
RightIdent.idr HCir a = refl

instance HCi : ∀{i} → Identity {lsuc i} (Set i → Set i) _∘_ id
HCi = record {}

HC : ∀{i} → Category {adj = lsuc} ((Set i) → (Set i)) (λ x → x ≡ (Set i → Set i)) _∘_
HC = record { ε = id } 