{-# OPTIONS --safe --without-K #-}

module Lib.Sigma.Properties where

open import Lib.Sigma.Type
open import Lib.Sigma.Base
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym
open import Lib.Bool.Type
open import Lib.Level

open import Lib.Dec.InstanceGenerators.IsSet

comm× : ∀{i j}{A : Set i}{B : Set j} → A × B ↔ B × A
comm× = swap , swap

assocΣ : ∀{i j k}{A : Set i}{B : A → Set j}{C : (a : A) → B a → Set k} → (Σ (Σ A B) λ ab → C (fst ab) (snd ab)) ↔ Σ A (λ a → Σ (B a) (C a))
fst assocΣ ((a , b) , c) = a , b , c
snd assocΣ (a , b , c) = (a , b) , c

assoc× : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A × B) × C ↔ A × B × C
assoc× = assocΣ

Σ-inj : ∀{i j}{A : Set i}{B : A → Set j}{a b : Σ A B} →
  (eq : a ≡ b) → (fst a ≡ fst b) × (subst B (cong fst eq) (snd a) ≡ snd b)
Σ-inj refl = refl , refl

infixr 4 _,=_
_,=_ : ∀{i j}{A : Set i}{B : A → Set j}{a b : A}{x : B a}{y : B b} →
  (eq : a ≡ b) → (eq2 : subst B eq x ≡ y) → _≡_ {A = Σ A B} (a , x) (b , y)
refl ,= refl = refl

×-inj : ∀{i j}{A : Set i}{B : Set j}{a b : A × B} →
  (a ≡ b) ↔ (fst a ≡ fst b) × (snd a ≡ snd b)
×-inj = (λ {refl → refl , refl}) , (λ {(refl , refl) → refl})

,-dec : ∀{i j}{A : Set i}{B : Set j}{a c : A}{b d : B} →
  Dec (a ≡ c) → Dec (b ≡ d) → Dec ((a , b) ≡ (c , d))
,-dec (no p) _ = no λ {refl → p refl}
,-dec (yes p) (no q) = no λ {refl → q refl}
,-dec (yes refl) (yes refl) = yes refl

×-dec : ∀{i j}{A : Set i}{B : Set j} → Dec A → Dec B → Dec (A × B)
×-dec (no p)  _       = no λ ab → p (fst ab)
×-dec (yes p) (no q)  = no λ ab → q (snd ab)
×-dec (yes p) (yes q) = yes (p , q)

Σ-dec : ∀{i j}{A : Set i}{B : A → Set j}
    → ((a b : A) → Dec (a ≡ b))
    → (∀{a} → ((c d : B a) → Dec (c ≡ d)))
    → (x y : Σ A B) → Dec (x ≡ y)
Σ-dec {i} {j} {A} {B} e1 e2 (x1 , x2) (y1 , y2) with e1 x1 y1
... | no b  = no λ e → b (cong fst e)
... | yes e with e2 (subst B e x2) y2
... | yes e4 = yes (e ,= e4)
... | no b = no λ e3 → let pr = Hedberg (DecProof e1) x1 y1 (cong fst e3) e in b (subst (λ biz → subst B biz x2 ≡ y2) pr (snd (Σ-inj {B = B} e3)))
