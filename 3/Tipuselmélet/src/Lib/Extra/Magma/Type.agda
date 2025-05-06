{-# OPTIONS --safe #-}
module Lib.Extra.Magma.Type where

open import Lib.Extra.Partial.Type public
open import Lib.Unit

record Magma {i} (A : Set i) (_<>_ : A → A → A) : Set i where
    field
        overlap {{partial}} : Partial A (λ _ → ⊤) _<>_
    open Partial partial public