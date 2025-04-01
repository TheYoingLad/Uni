{-# OPTIONS --safe #-}
module Lib.Extra.Class.Divisibility where

open import Lib.Equality using (_≡_)

record RightDiv {i} (A : Set i) (_∘_ : A → A → A) (_⁻¹ : A → A) : Set i where
    field
        rdiv₁ : (x y : A) → (y ∘ (x ⁻¹)) ∘ x ≡ y
        rdiv₂ : (x y : A) → (y ∘ x) ∘ (x ⁻¹) ≡ y

record LeftDiv {i} (A : Set i) (_∘_ : A → A → A) (_⁻¹ : A → A) : Set i where
    field
        ldiv₁ : (x y : A) → x ∘ ( (x ⁻¹) ∘ y) ≡ y
        ldiv₂ : (x y : A) → (x ⁻¹) ∘ ( x ∘ y) ≡ y
    

record Div {i} (A : Set i) (_∘_ : A → A → A) (_⁻¹ : A → A) : Set i where
    field
        overlap {{left}} : LeftDiv A _∘_ _⁻¹
        overlap {{right}} : RightDiv A _∘_ _⁻¹
    module left = LeftDiv left
    module right = RightDiv right
    open left public
    open right public
