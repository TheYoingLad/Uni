{-# OPTIONS --safe #-}
module Lib.Extra.Monoid.Type where

open import Lib.Extra.Semigroup.Type public
open import Lib.Extra.Class.Identity public

record Monoid {i} (A : Set i) (_<>_ : A → A → A) : Set i where
    field
        ε : A
        overlap {{semiGroup}} : SemiGroup A _<>_
        overlap {{identity}} : Identity A _<>_ ε
    module semiGroup = SemiGroup semiGroup
    module identity = Identity identity 
    open semiGroup public
    open identity public