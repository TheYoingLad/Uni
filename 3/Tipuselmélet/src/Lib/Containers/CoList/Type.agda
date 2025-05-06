{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoList.Type where

open import Lib.Unit.Type
open import Lib.Maybe.Type
open import Lib.Sigma.Type

record CoList {a}(A : Set a) : Set a where
  coinductive
  constructor mkCoList
  field
    uncons : Maybe (A × CoList A)

  head : Maybe A
  head with uncons
  ... | nothing = nothing
  ... | just (x , _) = just x

  tail : Maybe (CoList A)
  tail with uncons
  ... | nothing = nothing
  ... | just (_ , xs) = just xs

open CoList public

instance
  [] : ∀{a}{A : Set a} → CoList A
  uncons [] = nothing

infixr 5 _∷_
_∷_ : ∀{a}{A : Set a} → A → CoList A → CoList A
uncons (x ∷ xs) = just (x , xs)
