{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.Stream.Type where

infixr 5 _∷_
record Stream {a} (A : Set a) : Set a where
  coinductive
  constructor _∷_
  field
    head : A
    tail : Stream A

open Stream public
