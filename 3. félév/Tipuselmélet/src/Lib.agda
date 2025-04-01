{-# OPTIONS --safe --guardedness --without-K #-}

module Lib where

-- There are only container's types imported.
-- They have to be opened manually,
-- because there are a lot of functions that have the same name.

open import Lib.Class public
open import Lib.Level public
open import Lib.Function public
open import Lib.Inspect public
open import Lib.Reflection public
  renaming ( irrelevant to irrelevant-in-reflection )
open import Lib.Unit public
  renaming ( _≟_ to _≟⊤_ )
open import Lib.Empty public
  renaming ( _≟_ to _≟⊥_ )
open import Lib.Nat public
  renaming ( _≟_ to _≟ℕ_
           ; case to case-ℕ
           ; case-proof to case-ℕ-proof
           ; ite to ite-ℕ
           ; rec to rec-ℕ
           ; elim to elim-ℕ
           )
open import Lib.Bool public
  renaming ( contradiction to contradictionᵇ
           ; contraposition to contrapositionᵇ
           ; _≟_ to _≟ᵇ_
           ; elim to elim-Bool
           ; ind to ind-Bool
           )
open import Lib.CoNat public
  renaming ( coite to coite-ℕ∞
           ; _+_ to _+∞_
           ; add to add∞
           ; add' to add∞'
           ; _*_ to _*∞_
           ; idr+ to idr+∞
           ; idl+ to idl+∞
           ; _^_ to _^∞_
           ; embed to embed-ℕ∞
           )
open import Lib.Sum public
  renaming ( map to map-⊎
           ; mapᵣ to mapᵣ-⊎
           ; mapₗ to mapₗ-⊎
           ; elim to elim-⊎
           ; ind to ind-⊎
           ; swap to swap-⊎
           )
open import Lib.Fin public
  renaming ( cast to cast-Fin
           ; elim to elim-Fin
           )
open import Lib.CoFin public
  renaming ( coite to coite-CoFin
           ; coiteΣ to coite-CoFinΣ
           ; coiteιΣ to coite-CoFinιΣ
           )
open import Lib.IndCoFin public
  renaming ( embed to embed-IndCoFin
           ; cast to cast-IndCoFin
           )
open import Lib.Equality public
open import Lib.Dec public
open import Lib.Maybe public
  renaming ( elim to elim-Maybe
           ; ite to ite-Maybe
           ; ind to ind-Maybe
           ; map to map-Maybe
           )
open import Lib.Ordering public
  renaming ( _≟_ to _≟Ordering_
           ; case to case-Ordering
           ; ite to ite-Ordering
           ; elim to elim-Ordering
           ; ind to ind-Ordering
           )
open import Lib.UnitOrEmpty public
open import Lib.Pointed public
open import Lib.Isomorphism public
open import Lib.Lazy public
  renaming ( ite to ite-Lazy ) -- Functor, Applicative, Monad class needed and rename stuff in Base.
open import Lib.Relation public
open import Lib.TruncLevel public

------------------------------------------------------------
-- Change when needed
-- open import Lib.Product public
  -- renaming (map to map-×)
open import Lib.Sigma public
  renaming ( map to map-Σ
           ; swap to swap-×
           )

------------------------------------------------------------
-- Containers

open import Lib.Containers.List.Type hiding (module List) public
module List where
  open import Lib.Containers.List hiding (module List) public

open import Lib.Containers.Stream.Type hiding (module Stream) public
module Stream where
  open import Lib.Containers.Stream hiding (module Stream) public

open import Lib.Containers.CoList.Type hiding (module CoList; _∷_; []; head; tail) public
module CoList where
  open import Lib.Containers.CoList hiding (module CoList) public

open import Lib.Containers.Vector.Type hiding (module Vec) public
module Vec where
  open import Lib.Containers.Vector hiding (module Vec) public

open import Lib.Containers.CoVector.Type hiding (module CoVec; []) public
module CoVec where
  open import Lib.Containers.CoVector hiding (module CoVec) public

open import Lib.Containers.HList.Type hiding (module HList) public
module HList where
  open import Lib.Containers.HList hiding (module HList) public
