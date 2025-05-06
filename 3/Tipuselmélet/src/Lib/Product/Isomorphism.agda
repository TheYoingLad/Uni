{-# OPTIONS --safe --without-K #-}

module Lib.Product.Isomorphism where

open import Lib.Product.Type
open import Lib.Product.Properties
open import Lib.Isomorphism.Type
open import Lib.Equality.Type

×≅ : ∀{i j}{A : Set i}{B : Set j}{a1 a2 : A}{b1 b2 : B} → ((a1 , b1) ≡ (a2 , b2)) ≅ (a1 ≡ a2 × b1 ≡ b2)
×≅ {a1 = a1} {a2} {b1} {b2} = ≅-proof (fst ×≡) (snd ×≡) (λ {refl → refl}) λ {(refl , refl) → refl}
