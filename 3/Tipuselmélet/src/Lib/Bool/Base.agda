{-# OPTIONS --safe --without-K #-}

module Lib.Bool.Base where

open import Lib.Bool.Type
open import Lib.Equality.Type
open import Lib.Unit.Type
open import Lib.Empty.Type

infixr 3 _∧_
infixr 2 _∨_ _xor_
infixr 1 _⊃_

not : Bool → Bool
not true  = false
not false = true

-- \wedge = \and = ∧
_∧_ : Bool → Bool → Bool
true  ∧ b = b
false ∧ b = false

-- \vee = \or = ∨
_∨_ : Bool → Bool → Bool
true  ∨ b = true
false ∨ b = b

_xor_ : Bool → Bool → Bool
true  xor b = not b
false xor b = b

_⊃_ : Bool → Bool → Bool
true  ⊃ b = b
false ⊃ b = true

infix 0 if_then_else_
if_then_else_ : ∀{i}{A : Set i} → Bool → A → A → A
if false then t else f = f 
if true  then t else f = t

infix 0 ifₕ_then_else_
ifₕ_then_else_ : ∀{i}{A B : Set i} → (b : Bool) → A → B → if b then A else B
ifₕ_then_else_ false t f = f
ifₕ_then_else_ true  t f = t

elim : ∀{i}{A : Bool → Set i} → (b : Bool) → A true → A false → A b
elim false t f = f
elim true t f = t

ind : ∀{i}{A : Bool → Set i} → (b : Bool) → (b ≡ true → A b) → (b ≡ false → A b) → A b
ind false t f = f refl
ind true t f = t refl

T : Bool → Set
T false = ⊥
T true  = ⊤
