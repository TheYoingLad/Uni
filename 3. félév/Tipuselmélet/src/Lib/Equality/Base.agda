{-# OPTIONS --safe --without-K #-}

module Lib.Equality.Base where

open import Lib.Equality.Type
open import Lib.Sigma.Type
open import Lib.Sum.Type
open import Lib.Unit.Type
open import Lib.Empty.Type

sym : ∀{i}{A : Set i}{x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans eq1 refl = eq1

-- \sq5 = ◾

infixr 2 _◾_

_◾_ : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
_◾_ = trans

{-# DISPLAY trans = _◾_ #-}

subst : ∀{i j}{A : Set i}(P : A → Set j){x y : A} → x ≡ y → P x → P y
subst P refl px = px

substd₂ : ∀{i j k}{A : Set i}{B : A → Set j}(P : (a : A) → B a → Set k){x x' : A}{y : B x}{y' : B x'}(e : x ≡ x') → subst B e y ≡ y' → P x y → P x' y'
substd₂ P refl refl pxy = pxy

subst₂ : ∀{i j k}{A : Set i}{B : Set j}(P : A → B → Set k){x x' : A}{y y' : B} → x ≡ x' → y ≡ y' → P x y → P x' y'
subst₂ P {x} {x'} {y} e1 e2 pxy = subst (P x') e2 (subst (λ x → P x y) e1 pxy)

subst₃ : ∀{i j k l}{A : Set i}{B : Set j}{C : Set k}(P : A → B → C → Set l){x x' : A}{y y' : B}{z z' : C}
       → x ≡ x' → y ≡ y' → z ≡ z' → P x y z → P x' y' z'
subst₃ P {x} {x'} {y} {y'} {z} e1 e2 e3 pxyz = subst (P x' y') e3 (subst (λ y → P x' y z) e2 (subst (λ x → P x y z) e1 pxyz))

cast : ∀{i}{A B : Set i} → A ≡ B → A → B
cast = subst (λ a → a)

congd : ∀{i j}{A : Set i}(B : A → Set j)(f : (a : A) → B a){x y : A}(e : x ≡ y) → subst B e (f x) ≡ f y
congd _ _ refl = refl

congd₂ : ∀{i j k}{A : Set i}(B : A → Set j)(C : (a : A) → B a → Set k)(f : (a : A) → (b : B a) → C a b){x y : A}{bx : B x}{by : B y}(e : x ≡ y)(e2 : subst B e bx ≡ by) → substd₂ C e e2 (f x bx) ≡ f y by
congd₂ B C f refl refl = refl

cong : ∀{i j}{A : Set i}{B : Set j}(f : A → B){x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

cong₂ : ∀{i j k}{A : Set i}{B : Set j}{C : Set k}(f : A → B → C){x x' : A}{y y' : B} →
  x ≡ x' → y ≡ y' → f x y ≡ f x' y'
cong₂ f refl refl = refl

cong₃ : ∀{i j k l}{A : Set i}{B : Set j}{C : Set k}{D : Set l}(f : A → B → C → D){x x' : A}{y y' : B}{z z' : C} →
  x ≡ x' → y ≡ y' → z ≡ z' → f x y z ≡ f x' y' z'
cong₃ f refl refl refl = refl

-- -- \== = ≡
-- -- \< = ⟨
-- -- \> = ⟩
-- _≡⟨_⟩_ : ∀{i}{A : Set i}(x : A){y z : A} → x ≡ y → y ≡ z → x ≡ z
-- _ ≡⟨ p ⟩ q = trans p q

-- -- \qed = ∎
-- _∎ : ∀{i}{A : Set i}(x : A) → x ≡ x
-- _ ∎ = refl

-- infixr 2 _≡⟨_⟩_
-- infix 3 _∎

J : ∀{i j}{A : Set i}{x : A}(B : (y : A) → x ≡ y → Set j)(bx : B x refl){y : A}(e : x ≡ y) → B y e
J B bx refl = bx

happly : ∀{i j}{A : Set i}{B : A → Set j}{f g : (a : A) → B a} → f ≡ g → ∀ x → f x ≡ g x
happly e x = cong (λ t → t x) e

sym-by-subst : ∀{i}{A : Set i}{x y : A} → x ≡ y → y ≡ x
sym-by-subst {x = x} {y} eq = subst (λ a → a ≡ x) eq refl

cong-by-subst : ∀{i j}{A : Set i}{B : Set j}(f : A → B){x y : A} → x ≡ y → f x ≡ f y
cong-by-subst f {x} {y} eq = subst (λ a → f x ≡ f a) eq refl

trans-by-subst : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans-by-subst {x = x} {y} {z} eq1 eq2 = subst (λ a → x ≡ a) eq2 eq1
