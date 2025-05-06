{-# OPTIONS --safe #-}
module Lib.Extra.Functor.Endo.Type where

open import Agda.Primitive using (Level)
open import Lib.Extra.Category.Type
open import Lib.Extra.Functor.Type

record EndoFunctor {i : Level}{adj : Level → Level}{A : Set i}{_⇛_ : A → A → A }(P : Set i → Set (adj i)) {{a : P A}}(C : Category {adj = adj} A P _⇛_ {{a}}) : Set (i) where
    field
        overlap {{functor}} : Functor {i} {i} {adj} {A} {A} {_⇛_} {_⇛_} P P {{a}} {{a}} C C
    module category = Category C
    module functor = Functor functor
    open functor public