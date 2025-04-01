{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Equality.RelationToEqualityType where

open import Lib.Maybe.Equality.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties
open import Lib.Maybe.Type
open import Lib.Maybe.Base
open import Lib.Maybe.Properties

≡Maybe→≡ : ∀{i}{A : Set i}(x y : Maybe A) → x ≡Maybe y → x ≡ y
≡Maybe→≡ nothing nothing e = refl
≡Maybe→≡ (just x) (just y) e = cong just e

≡→≡Maybe : ∀{i}{A : Set i}(x y : Maybe A) → x ≡ y → x ≡Maybe y
≡→≡Maybe nothing nothing e = _
≡→≡Maybe (just x) (just y) e = just-injective e

≡Maybe→≡→≡Maybe-id : ∀{i}{A : Set i}(x y : Maybe A)(e : x ≡Maybe y) → ≡→≡Maybe x y (≡Maybe→≡ x y e) ≡ e
≡Maybe→≡→≡Maybe-id nothing nothing e = refl
≡Maybe→≡→≡Maybe-id (just x) (just y) e = cong∘ (fromMaybe x) just e ◾ cong-id e

≡→≡Maybe→≡-id : ∀{i}{A : Set i}(x y : Maybe A)(e : x ≡ y) → ≡Maybe→≡ x y (≡→≡Maybe x y e) ≡ e
≡→≡Maybe→≡-id nothing .nothing refl = refl
≡→≡Maybe→≡-id (just x) .(just x) refl = refl
