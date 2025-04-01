{-# OPTIONS --safe #-}
module Lib.Extra.Semigroupoid.Type where

open import Agda.Primitive using (Level)
open import Lib.Extra.Partial.Type public
open import Lib.Extra.Class.Associtivity public

record Semigroupoid {i} {adj : Level → Level} (A : Set i) (P : Set  i → Set (adj i)) (_<>_ : A → A → A) {{m : P A}} : Set i where
    field
        overlap {{magma}} : Partial {adj = adj} A P _<>_
        overlap {{assoc}} : Assoc A _<>_
    module magma = Partial magma
    module assoc = Assoc assoc
    open magma public
    open assoc public