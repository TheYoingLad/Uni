module zh1gyak where

open import Lib

iteNat : {C : Set} → C → (C → C) → ℕ → C
iteNat czero csuc zero = czero
iteNat czero csuc (suc n) = csuc (iteNat czero csuc n)

f : ℕ → ℕ
f zero = 2
f (suc n) = 3 + f n

f' : ℕ → ℕ
f' n = iteNat 2 (λ x → 3 + x) n

{-
f 2
3 + f 1
3 + 3 + f 0
3 + 3 + 2
8

f' 2
iteNat 2 (λ x → 3 + x) (suc 1)
(λ x → 3 + x) (iteNat 2 (λ x → 3 + x) (suc 0))
(λ x → 3 + x) ((λ x → 3 + x) (iteNat 2 (λ x → 3 + x) 0))
(λ x → 3 + x) ((λ x → 3 + x) 2)
(λ x → 3 + x) (3 + 2)
(λ x → 3 + x) 5
(3 + 5)
8
-}

_++_ : {A : Set} → List A → List A → List A
_++_ = Lib.List._++_

concat : {A : Set} → List (List A) → List A
concat [] = [] 
concat (x ∷ xs) = x ++ concat xs

intersperse : {A : Set} → Stream A → A → Stream A
head (intersperse x a) = head x
tail (intersperse x a) = a ∷ (intersperse x a)

eqviv : (Bool → Bool) ↔ Bool × Bool
fst (eqviv x) = {!   !}
snd (eqviv x) = {!   !}


data TriEither (A B C : Set) : Set where
    left : A → TriEither A B C
    middle : B → TriEither A B C
    right : C → TriEither A B C
--open TriEither 