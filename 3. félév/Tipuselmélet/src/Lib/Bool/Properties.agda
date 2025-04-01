{-# OPTIONS --safe --without-K #-}

module Lib.Bool.Properties where

open import Lib.Dec
open import Lib.Dec.PatternSynonym
open import Lib.Bool.Type
open import Lib.Bool.Base
open import Lib.Equality

infix 4 _≟_
_≟_ : (a b : Bool) → Dec (a ≡ b)
true  ≟ true  = yes refl
false ≟ false = yes refl
true  ≟ false = no λ ()
false ≟ true  = no λ ()

reduce-notnot : ∀{b} → not (not b) ≡ b
reduce-notnot {false} = refl
reduce-notnot {true} = refl

ass∧ : ∀{a b c} → ((a ∧ b) ∧ c) ≡ (a ∧ (b ∧ c))
ass∧ {false} = refl
ass∧ {true} = refl

comm∧ : ∀{a b} → (a ∧ b) ≡ (b ∧ a)
comm∧ {false} {false} = refl
comm∧ {false} {true} = refl
comm∧ {true} {false} = refl
comm∧ {true} {true} = refl

idem∧ : ∀{b} → (b ∧ b) ≡ b
idem∧ {false} = refl
idem∧ {true} = refl

ass∨ : ∀{a b c} → ((a ∨ b) ∨ c) ≡ (a ∨ (b ∨ c))
ass∨ {false} = refl
ass∨ {true} = refl

comm∨ : ∀{a b} → (a ∨ b) ≡ (b ∨ a)
comm∨ {false} {false} = refl
comm∨ {false} {true} = refl
comm∨ {true} {false} = refl
comm∨ {true} {true} = refl

idem∨ : ∀{b} → (b ∨ b) ≡ b
idem∨ {false} = refl
idem∨ {true} = refl

dist∨∧ : ∀{a b c} → ((a ∨ b) ∧ c) ≡ (a ∧ c ∨ b ∧ c)
dist∨∧ {false} = refl
dist∨∧ {true} {_} {false} = comm∧ {false}
dist∨∧ {true} {_} {true} = refl

dist∧∨ : ∀{a b c} → ((a ∧ b) ∨ c) ≡ ((a ∨ c) ∧ (b ∨ c))
dist∧∨ {false} {_} {false} = refl
dist∧∨ {false} {_} {true} = comm∨ {true}
dist∧∨ {true} = refl

⊃→∨ : ∀{a b} → (a ⊃ b) ≡ (not a ∨ b)
⊃→∨ {false} = refl
⊃→∨ {true} = refl

null⊃ : ∀{a} → (a ⊃ true) ≡ true
null⊃ {false} = refl
null⊃ {true} = refl

pierce : ∀{a b} → (((a ⊃ b) ⊃ a) ⊃ a) ≡ true
pierce {false} = refl
pierce {true} = null⊃

lem : ∀{a} → (a ∨ not a) ≡ true
lem {false} = refl
lem {true} = refl

contradiction : ∀{a} → (a ∧ not a) ≡ false
contradiction {false} = refl
contradiction {true} = refl

contraposition : ∀{a b} → (a ⊃ b) ≡ (not b ⊃ not a)
contraposition {false} = sym null⊃
contraposition {true} {false} = refl
contraposition {true} {true} = refl

demorgan∨ : ∀{a b} → not (a ∨ b) ≡ (not a ∧ not b)
demorgan∨ {false} = refl
demorgan∨ {true} = refl

demorgan∧ : ∀{a b} → not (a ∧ b) ≡ (not a ∨ not b)
demorgan∧ {false} = refl
demorgan∧ {true} = refl

Bool-η : ∀{x} → (if x then true else false) ≡ x
Bool-η {false} = refl
Bool-η {true} = refl
