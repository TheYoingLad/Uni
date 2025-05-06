{-# OPTIONS --safe #-}
module Lib.Extra.Semigroup.Type where

open import Lib.Extra.Magma.Type public
open import Lib.Extra.Class.Associtivity public

record SemiGroup {i} (A : Set i) (_<>_ : A → A → A) : Set i where
    field
        overlap {{magma}} : Magma A _<>_
        overlap {{assoc}} : Assoc A _<>_
    module magma = Magma magma
    module assoc = Assoc assoc
    open magma public
    open assoc public