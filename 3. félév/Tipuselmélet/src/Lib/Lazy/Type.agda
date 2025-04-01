{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Lazy.Type where

data Lazy {i}(A : Set i) : Set i
record Lazy∞ {i}(A : Set i) : Set i

data Lazy A where
  now   : A → Lazy A
  later : Lazy∞ A → Lazy A

record Lazy∞ A where
  coinductive
  constructor delay
  field
    force : Lazy A

open Lazy∞ public
