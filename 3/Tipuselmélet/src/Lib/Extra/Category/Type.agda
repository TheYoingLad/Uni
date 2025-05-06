{-# OPTIONS --safe #-}
module Lib.Extra.Category.Type where

open import Agda.Primitive using (Level)
open import Lib.Extra.Semigroupoid.Type public
open import Lib.Extra.Class.Identity public

record Category {i} {adj : Level → Level} (A : Set i) (P : Set i → Set ( adj i )) (_<>_ : A → A → A) {{m : P A}} : Set i where
    field
        ε : A
        overlap {{groupoid}} : Semigroupoid {adj = adj} A P _<>_
        overlap {{id}} : Identity A _<>_ ε
    module groupoid = Semigroupoid groupoid
    open groupoid public
    module id = Identity id 
    open id public