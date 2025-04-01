{-# OPTIONS --safe #-}
module Lib.Extra.Functor.Type where

open import Lib using (lsuc; Level; _⊔_; Σ; _≡_; _∘_)
open import Lib.Extra.Category.Type

-- F : Functor
-- F (idₓ) = id_F(x)
-- F (g ∘ f) = F(g) ∘ F(f)

record Functor {i j : Level}{adj : Level → Level}{A : Set i}{B : Set j}{_⇛_ : A → A → A }{_↠_ : B → B → B}(P : Set i → Set (adj i)) (Q : Set j  → Set (adj j)) {{a : P A}}{{b : Q B}}(C : Category {adj = adj} A P _⇛_ {{a}})(D : Category {adj = adj} B Q _↠_ {{b}}) : Set (i ⊔ j) where
    field
      [_↑] : A → B
      [↓_] : (A → A → A) → (B → B → B)
      idp : [↓ (λ x _ → x) ] ≡ λ x _ → x
      ∘p : {a b : B}{f g : A → A → A} → [↓ (λ x y → f x (g x y)) ] a b ≡ (λ c d → [↓ f ] c ([↓ g ] c d)) a b
    module C = Category C
    module D = Category D

  