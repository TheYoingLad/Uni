{-# OPTIONS --safe --without-K #-}

module Lib.Irrelevant where

record Irrelevant {a} (A : Set a) : Set a where
  constructor [_]
  field
    .irrelevant : A

open Irrelevant public
