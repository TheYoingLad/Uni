{-# OPTIONS --safe --without-K #-}

module Lib.Containers.CoList.PatternSynonym where

open import Lib.Maybe.Type
open import Lib.Sigma.Type

infixr 5 _∷∞_
pattern []∞ = nothing
pattern _∷∞_ x xs = just (x , xs)
