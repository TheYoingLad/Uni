{-# OPTIONS --safe --without-K #-}

open import Agda.Primitive
open import Lib.Relation.Base

module Lib.Relation.Notation {ℓ} {A : Set ℓ}
  (R : A → A → Set ℓ)
  (trans : Transitive R)
  (refl : Reflexive R)
  where


-- \== = ≡
-- \< = ⟨
-- \> = ⟩
_≡⟨_⟩_ : (x : A){y z : A} → R x y → R y z → R x z
_ ≡⟨ p ⟩ q = trans p q

-- \qed = ∎
_∎ : (x : A) → R x x
_ ∎ = refl

infixr 2 _≡⟨_⟩_
infix 3 _∎
