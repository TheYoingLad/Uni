{-# OPTIONS --safe #-}
module Lib.Extra.Partial.Type where

open import Agda.Primitive using (Level )

record Partial {i} {adj : Level → Level} (A : Set i) (P : Set i → Set (adj i)) (_<>_ : A → A → A) {{_ : P A}} : Set i where
    _∘_ = _<>_ 