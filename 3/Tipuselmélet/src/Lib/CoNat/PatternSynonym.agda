{-# OPTIONS --safe --without-K #-}

module Lib.CoNat.PatternSynonym where

open import Lib.Maybe.Type

pattern zero∞ = nothing
pattern suc∞ n = just n
