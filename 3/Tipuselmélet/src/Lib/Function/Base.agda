{-# OPTIONS --safe --without-K #-}

module Lib.Function.Base where

open import Agda.Primitive

infix -1000 Π
Π : ∀{a b}(A : Set a) → (A → Set b) → Set (a ⊔ b)
Π A B = (a : A) → B a

typeOf : ∀{a}{A : Set a} → A → Set a
typeOf {A = A} _ = A

the : ∀{a}(A : Set a) → A → A
the _ a = a

id : ∀{a}{A : Set a} → A → A
id {A = A} a = the A a

const : ∀{a b}{A : Set a}{B : Set b} → A → B → A
const x = λ _ → x

constᵣ : ∀{a b}{A : Set a}{B : Set b} → A → B → B
constᵣ _ = id

infixr 9 _⊚_ _∘_
infixl 0 _|>_ _|>'_
infixr -1 _$_ _$'_

_⊚_ : ∀ {a b c} {A : Set a} {B : A → Set b} {C : {x : A} → B x → Set c} →
      (∀ {x} (y : B x) → C y) → (g : (x : A) → B x) →
      ((x : A) → C (g x))
f ⊚ g = λ x → f (g x)
{-# INLINE _⊚_ #-}

_∘_ : ∀{a b c}{A : Set a}{B : Set b}{C : Set c} → (B → C) → (A → B) → (A → C)
f ∘ g = f ⊚ g
{-# INLINE _∘_ #-}

flip : ∀ {a b c} {A : Set a} {B : Set b} {C : A → B → Set c} →
       ((x : A) (y : B) → C x y) → ((y : B) (x : A) → C x y)
flip f = λ y x → f x y
{-# INLINE flip #-}

_$_ : ∀ {a b} {A : Set a} {B : A → Set b} →
      ((x : A) → B x) → ((x : A) → B x)
f $ x = f x
{-# INLINE _$_ #-}

_|>_ : ∀ {a b} {A : Set a} {B : A → Set b} →
       (a : A) → (∀ a → B a) → B a
_|>_ = flip _$_
{-# INLINE _|>_ #-}

_$- : ∀ {a b} {A : Set a} {B : A → Set b} → ((x : A) → B x) → ({x : A} → B x)
f $- = f _
{-# INLINE _$- #-}

λ- : ∀ {a b} {A : Set a} {B : A → Set b} → ({x : A} → B x) → ((x : A) → B x)
λ- f = λ _ → f
{-# INLINE λ- #-}

it : ∀{a}{A : Set a} → ⦃ a : A ⦄ → A
it ⦃ x ⦄ = x

flip' : ∀{a b c}{A : Set a}{B : Set b}{C : Set c} → (A → B → C) → (B → A → C)
flip' = flip

_$'_ : ∀{a b}{A : Set a}{B : Set b} → (A → B) → (A → B)
_$'_ = _$_

_|>'_ : ∀{a b}{A : Set a}{B : Set b} → A → (A → B) → B
_|>'_ = _|>_
