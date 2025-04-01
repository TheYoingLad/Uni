{-# OPTIONS --safe --without-K #-}

module Lib.Equality.Properties where

open import Lib.Equality.Type
open import Lib.Equality.Base

open import Lib.Dec.Type
open import Lib.Sum.Type
open import Lib.Empty.Type
open import Lib.Empty.Base
open import Lib.Dec.PatternSynonym

open import Lib.Level

idl≡ : ∀{i}{A : Set i}{x y : A}(e : x ≡ y) → trans refl e ≡ e
idl≡ refl = refl

invr≡ : ∀{i}{A : Set i}{x y : A}(e : x ≡ y) → trans e (sym e) ≡ refl
invr≡ refl = refl

invl≡ : ∀{i}{A : Set i}{x y : A}(e : x ≡ y) → trans (sym e) e ≡ refl
invl≡ refl = refl

involutive-sym : ∀{i}{A : Set i}{x y : A}(e : x ≡ y) → sym (sym e) ≡ e
involutive-sym refl = refl

dist-sym-trans : ∀{i}{A : Set i}{x y z : A}(e1 : x ≡ y)(e2 : y ≡ z) → sym (trans e1 e2) ≡ trans (sym e2) (sym e1)
dist-sym-trans e1 refl = sym (idl≡ (sym e1))

dist-sym-cong : ∀{i j}{A : Set i}{B : Set j}(f : A → B){a1 a2 : A}(e : a1 ≡ a2) → sym (cong f e) ≡ cong f (sym e)
dist-sym-cong f refl = refl

dist-cong-trans : ∀{i j}{A : Set i}{B : Set j}(f : A → B){a a' a'' : A}(e1 : a ≡ a')(e2 : a' ≡ a'') → cong f (trans e1 e2) ≡ trans (cong f e1) (cong f e2)
dist-cong-trans f e1 refl = refl

assoc-trans : ∀{i}{A : Set i}{a b c d : A}(eq1 : a ≡ b)(eq2 : b ≡ c)(eq3 : c ≡ d) →
  trans (trans eq1 eq2) eq3 ≡ trans eq1 (trans eq2 eq3)
assoc-trans _ _ refl = refl

trans-cancelr : ∀{i}{A : Set i}{a b : A}(p : a ≡ b)(q : b ≡ b) → trans p q ≡ p → q ≡ refl
trans-cancelr p q e = sym (idl≡ q)
                   ◾ cong (_◾ q) (sym (invl≡ p))
                   ◾ assoc-trans (sym p) p q
                   ◾ cong (sym p ◾_) e
                   ◾ invl≡ p

trans-cancell : ∀{i}{A : Set i}{a b : A}(p : a ≡ a)(q : a ≡ b) → trans p q ≡ q → p ≡ refl
trans-cancell p q e = cong (trans p) (sym (invr≡ q))
                   ◾ sym (assoc-trans p q (sym q))
                   ◾ cong (_◾ sym q) e
                   ◾ invr≡ q

cong∘ : ∀{i j}{A : Set i}{B : Set j}(f : B → A)(g : A → B) →
  {a b : A}(eq : a ≡ b) → cong f (cong g eq) ≡ cong (λ x → f (g x)) eq
cong∘ f g refl = refl

cong-id : ∀{i}{A : Set i}{a b : A} →
  (eq : a ≡ b) → cong (λ x → x) eq ≡ eq
cong-id refl = refl

cong$ : ∀{i j}{A : Set i}{B : Set j}(f f' : A → B){a a' : A} → f ≡ f' → a ≡ a' → f a ≡ f' a'
cong$ _ _ = cong₂ (λ x → x)

substconst  : ∀{ℓ}{A : Set ℓ}{ℓ'}{B : Set ℓ'}{a a' : A}(e : a ≡ a'){b : B} →
  subst (λ _ → B) e b ≡ b
substconst refl = refl

substd₂const : ∀{i}{A : Set i}{j}{B : A → Set j}{k}{C : Set k}{a a' : A}(e : a ≡ a'){b : B a}{b' : B a'}(e2 : subst B e b ≡ b'){c : C} → substd₂ (λ _ _ → C) e e2 c ≡ c
substd₂const refl refl = refl

substcong : ∀{ℓ ℓ' ℓ''}{A : Set ℓ}{B : Set ℓ'}(C : B → Set ℓ'')(f : A → B) → 
  {a a' : A}(e : a ≡ a'){c : C (f a)} → 
  subst {A = B} C (cong f e) c ≡ subst {A = A} (λ a → C (f a)) e c
substcong C f refl = refl

substΠ : ∀{ℓ ℓ' ℓ''}{A : Set ℓ}{B : Set ℓ'}(C : A → B → Set ℓ'') →
  {a a' : A}(e : a ≡ a'){f : (b : B) → C a b} → 
  subst (λ a → (b : B) → C a b) e f ≡ λ b → subst (λ a → C a b) e (f b)
substΠ C refl = refl

substtrans : ∀{ℓ}{A : Set ℓ}{ℓ'}(P : A → Set ℓ'){a a' a'' : A}(e : a ≡ a')(e' : a' ≡ a''){p : P a} →
  subst P (trans e e') p ≡ subst P e' (subst P e p)
substtrans P e refl = refl

subst→ : ∀{ℓ ℓ' ℓ''}{A : Set ℓ}{B : Set ℓ'}(C : A → Set ℓ''){a a' : A}(e : a ≡ a'){f : B → C a} →
  subst (λ a → B → C a) e f ≡ λ b → subst C e (f b)
subst→ C refl = refl

subst$ : ∀{ℓ ℓ' ℓ''}{A : Set ℓ}{B : A → Set ℓ'}{C : A → Set ℓ''}
  (f : ∀ a → B a → C a){a a' : A}(e : a ≡ a'){b : B a} → 
  f a' (subst B e b) ≡ subst C e (f a b) 
subst$ f refl = refl

subst-concat-r : ∀{i}{A : Set i}{a : A} {x y : A} → (p : x ≡ y) → (q : a ≡ x) →
      subst (a ≡_) p q ≡ trans q p
subst-concat-r refl q = refl

subst-concat-l : ∀{i}{A : Set i}{a : A} {x y : A} → (p : x ≡ y) → (q : x ≡ a) →
      subst (_≡ a) p q ≡ trans (sym p) q
subst-concat-l refl q = sym (idl≡ q)

axiomK→isSet : ∀{i}{A : Set i} → ((a : A)(P : a ≡ a → Set i) → P refl → (p : a ≡ a) → P p) → (a b : A)(p q : a ≡ b) → p ≡ q
axiomK→isSet f a .a p refl = f a (_≡ refl) refl p
