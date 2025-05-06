{-# OPTIONS --safe --without-K #-}

module Lib.Class.IsTrunc where

open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Equality.Properties

open import Lib.Nat.Type
open import Lib.Nat.Base
open import Lib.Nat.Properties

open import Lib.Sigma.Type

open import Lib.Isomorphism.Type
open import Lib.Isomorphism.Base
open import Lib.Isomorphism.Properties

open import Lib.Level

open import Lib.TruncLevel.Type
open import Lib.TruncLevel.Base
open import Lib.TruncLevel.Properties
-- open import Lib.TruncLevel.Literals

isContrType : ∀{i}(A : Set i) → Set i
isContrType A = Σ A (λ a → (x : A) → a ≡ x)

isContrType→≡ : ∀{i}{A : Set i} → isContrType A → {x y : A} → x ≡ y
isContrType→≡ C {x} {y} = sym (snd C x) ◾ snd C y

isTruncType : ∀{i}(n : TruncLevel) → Set i → Set i
isTruncType ⟨-2⟩ A = isContrType A
isTruncType (suc n) A = (x y : A) → isTruncType n (x ≡ y)

{-
isTruncType : ∀{i}(n : ℕ) → Set i → Set i
isTruncType zero A = isContrType A
isTruncType (suc n) A = (x y : A) → isTruncType n (x ≡ y)
-}
sucIsTruncType : ∀{i}{A : Set i}(n : TruncLevel) → isTruncType n A → isTruncType (suc n) A
fst (sucIsTruncType ⟨-2⟩ H x y) = isContrType→≡ H
snd (sucIsTruncType ⟨-2⟩ H x .x) refl = invl≡ (snd H x)
sucIsTruncType (suc n) H x y p q = sucIsTruncType n (H x y) p q

raiseIsTruncType : ∀{i}{A : Set i}{n : TruncLevel}(k : ℕ) → isTruncType n A → isTruncType (raiseLevel k n) A
raiseIsTruncType zero H = H
raiseIsTruncType {n = n} (suc k) H x y = raiseIsTruncType k (sucIsTruncType n H x y)

record IsTrunc (n : TruncLevel){i}(A : Set i) : Set i where
  constructor isTrunc-proof
  field
    isTrunc : isTruncType n A

open IsTrunc {{...}} public

instance
  SucIsTrunc : ∀{i}{A : Set i}{n : TruncLevel} → ⦃ IsTrunc n A ⦄ → IsTrunc (suc n) A
  SucIsTrunc {n = n} ⦃ H ⦄ = isTrunc-proof (sucIsTruncType n (isTrunc ⦃ H ⦄))

RaiseIsTrunc : ∀{i}{A : Set i}{n : TruncLevel}(k : ℕ) → ⦃ IsTrunc n A ⦄ → IsTrunc (raiseLevel k n) A
RaiseIsTrunc zero ⦃ H ⦄ = H
RaiseIsTrunc {A = A} (suc k) ⦃ H ⦄ = subst (λ z → IsTrunc z A) (sucr-raiseLevel k _) (RaiseIsTrunc k ⦃ SucIsTrunc ⦃ H ⦄ ⦄)
{-
pr : ∀{i j}{A : Set i}{B : Set j}(iso : A ≅ B)(x y : B) → (coe← iso x ≡ coe← iso y) ≅ (x ≡ y)
pr iso x y = ≅-proof (coe←-injective iso) (cong (coe← iso)) (pr1 iso x y) λ where refl → cong (sym (id→← iso x) ◾_) (idl≡ (id→← iso x)) ◾ invl≡ (id→← iso x)
  where
    pr1 : ∀{i j}{A : Set i}{B : Set j}(iso : A ≅ B)(x y : B)(e : coe← iso x ≡ coe← iso y) → cong (coe← iso) (coe←-injective iso e) ≡ e
    pr1 iso x y e with coe←-injective iso e
    pr1 iso x .x e | refl = {!!}
    -- dist-cong-trans (coe← iso) (sym (id→← iso x)) (cong (coe→ iso) e ◾ id→← iso y) ◾ {!!}
-}
isoClosedContr→ : ∀{i j}{A : Set i}{B : Set j} → A ≅ B → isContrType A → isContrType B
isoClosedContr→ iso (a , p) = coe→ iso a , λ b → cong (coe→ iso) (p (coe← iso b)) ◾ id→← iso b

isoClosedContr← : ∀{i j}{A : Set i}{B : Set j} → A ≅ B → isContrType B → isContrType A
isoClosedContr← iso (b , p) = coe← iso b , λ a → cong (coe← iso) (p (coe→ iso a)) ◾ id←→ iso a

{-
isoClosedTrunc→ : ∀{i j}{A : Set i}{B : Set j}{n : ℕ} → A ≅ B → isTruncType n A → isTruncType n B
isoClosedTrunc→ {n = zero} = isoClosedContr→
isoClosedTrunc→ {n = suc n} iso f x y = let alma = f (coe← iso x) (coe← iso y) in isoClosedTrunc→ {n = n} {!!} alma

isoClosedTrunc← : ∀{i j}{A : Set i}{B : Set j}{n : ℕ} → A ≅ B → isTruncType n B → isTruncType n A
isoClosedTrunc← {n = n} iso b = {!!}
-}
-- λ (b , p) →
