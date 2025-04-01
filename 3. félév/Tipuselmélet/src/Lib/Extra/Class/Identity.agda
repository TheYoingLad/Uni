{-# OPTIONS --safe #-}
module Lib.Extra.Class.Identity where

open import Lib.Equality using (_≡_)

record LeftIdent {i} (A : Set i) (_∘_ : A → A → A) (ε : A) : Set i where
    field
      idl : (a : A) → ε ∘ a ≡ a

record RightIdent {i} (A : Set i) (_∘_ : A → A → A) (ε : A) : Set i where
    field
      idr : (a : A) → a ∘ ε ≡ a

record Identity {i} (A : Set i) (_∘_ : A → A → A) (ε : A) : Set i where
    field
      overlap {{right}} : RightIdent A _∘_ ε
      overlap {{left}} : LeftIdent A _∘_ ε
    module right = RightIdent right
    module left = LeftIdent left
    open right public
    open left public
