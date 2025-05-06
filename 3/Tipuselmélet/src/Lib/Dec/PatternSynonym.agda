{-# OPTIONS --safe --without-K #-}

module Lib.Dec.PatternSynonym where

open import Lib.Sum.Type

pattern no x = inr x
pattern yes x = inl x
