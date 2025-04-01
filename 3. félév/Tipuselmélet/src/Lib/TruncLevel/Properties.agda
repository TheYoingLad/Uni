{-# OPTIONS --safe --without-K #-}

module Lib.TruncLevel.Properties where

open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.TruncLevel.Type
open import Lib.TruncLevel.Base
open import Lib.Nat.Type

sucr-raiseLevel : (n : ℕ)(k : TruncLevel) → raiseLevel n (suc k) ≡ suc (raiseLevel n k)
sucr-raiseLevel zero k = refl
sucr-raiseLevel (suc n) k = cong suc (sucr-raiseLevel n k)
