{-# OPTIONS --no-postfix-projections #-}

module ZH2 where

open import Lib hiding (T)
open Vec
open import Lib.Nat.Properties

0' : ℕ
0' = zero

1' : ℕ
1' = suc zero

2' : ℕ
2' = suc 1'

3' : ℕ
3' = suc 2'

4' : ℕ
4' = suc 3'

5' : ℕ
5' = suc 4'

6' : ℕ
6' = suc 5'

7' : ℕ
7' = suc 6'

8' : ℕ
8' = suc 7'

9' : ℕ
9' = suc 8'

10' : ℕ
10' = suc 9'

11' : ℕ
11' = suc 10'

-- 1.

insertions : {A : Set}{n : ℕ} → A → Vec A n → Vec (Vec A (suc n)) (suc n)
insertions {A} {zero} a as = (a ∷ []) ∷ []
insertions {A} {suc n} a as = (a ∷ as) ∷ Vec.map ((Vec.head as) ∷_) (insertions a (Vec.tail as))

insertions-test1 : insertions {ℕ} 0' (1' ∷ 2' ∷ 3' ∷ []) ≡
  (0' ∷ 1' ∷ 2' ∷ 3' ∷ []) ∷
  (1' ∷ 0' ∷ 2' ∷ 3' ∷ []) ∷
  (1' ∷ 2' ∷ 0' ∷ 3' ∷ []) ∷
  (1' ∷ 2' ∷ 3' ∷ 0' ∷ []) ∷ []
insertions-test1 = refl

insertions-test2 : insertions true (false ∷ []) ≡ (true ∷ false ∷ []) ∷ (false ∷ true ∷ []) ∷ []
insertions-test2 = refl

insertions-test3 : insertions {ℕ} {4'} 10' (9' ∷ 11' ∷ 3' ∷ 0' ∷ []) ≡
  (10' ∷ 9' ∷ 11' ∷ 3' ∷ 0' ∷ []) ∷
  (9' ∷ 10' ∷ 11' ∷ 3' ∷ 0' ∷ []) ∷
  (9' ∷ 11' ∷ 10' ∷ 3' ∷ 0' ∷ []) ∷
  (9' ∷ 11' ∷ 3' ∷ 10' ∷ 0' ∷ []) ∷
  (9' ∷ 11' ∷ 3' ∷ 0' ∷ 10' ∷ []) ∷ []
insertions-test3 = refl

insertions-test4 : insertions {ℕ} {5'} 2' (0' ∷ 1' ∷ 5' ∷ 10' ∷ 3' ∷ []) ≡
  (2' ∷ 0' ∷ 1' ∷ 5' ∷ 10' ∷ 3' ∷ []) ∷
  (0' ∷ 2' ∷ 1' ∷ 5' ∷ 10' ∷ 3' ∷ []) ∷
  (0' ∷ 1' ∷ 2' ∷ 5' ∷ 10' ∷ 3' ∷ []) ∷
  (0' ∷ 1' ∷ 5' ∷ 2' ∷ 10' ∷ 3' ∷ []) ∷
  (0' ∷ 1' ∷ 5' ∷ 10' ∷ 2' ∷ 3' ∷ []) ∷
  (0' ∷ 1' ∷ 5' ∷ 10' ∷ 3' ∷ 2' ∷ []) ∷ []
insertions-test4 = refl

-- 2.

logical1 : (A B : Set) → ¬ (¬ (((A → B) → A) → A))
logical1 A B x = x (λ f → f (λ a → exfalso (x (λ _ → a))))

-- 3.

logical2 : Dec ((A : Set)(P : A → Set) → ((Σ A P) → ((a : A) → P a)))
logical2 = inr (λ x → x Bool P (true , tt) false) where
  P : Bool → Set
  P true = ⊤
  P false = ⊥

logical3 : Dec ((A : Set)(P : A → Set) → (((a : A) → P a) → (Σ A P)))
logical3 = inr λ x → fst (x ⊥ P λ {()}) where
  P : ⊥ → Set
  P ()

logical3' : Dec ((A : Set)(P : A → Set) → (((a : A) → P a) → (Σ A P)))
logical3' = inl λ A P x → _ , (x _)

pr7 : (n k : ℕ) → (n ^ n) * 0' + k ^ 1' ≡ k + 0'
pr7 n k = trans (cong (_+ k * 1') (nullr* (n ^ n))) 
  (trans (idr* k) (sym (idr+ k)))





-- elméleti dolgok

data T (A B : Set) : Set where
  T1 : (a : A) → T A B → T A B
  T2 : (n : ℕ) → Fin n → B → T A B

iteT : {A B C : Set}(t1 : (a : A) → C → C)(t2 : (n : ℕ) → Fin n → B → C) → T A B → C
iteT t1 t2 (T1 a x) = t1 a (iteT t1 t2 x)
iteT t1 t2 (T2 n x b) = t2 n x b

ηT : {A B C : Set}(t1 : (a : A) → C → C)(t2 : (n : ℕ) → Fin n → B → C) →
  (f : {A B C : Set} → T A B → C) → 
  (et1 : (a : A)(x : T A B) → f (T1 a x) ≡ t1 a (f x)) → 
  (et2 : (n : ℕ)(b : B)(x : Fin n) → f (T2 n x b) ≡ t2 n x b) → 
  (x : T A B) → f x ≡ iteT t1 t2 x
ηT t1 t2 f et1 et2 (T1 a x) = trans (et1 a x) (cong (t1 a) (ηT t1 t2 f et1 et2 x))
ηT t1 t2 f et1 et2 (T2 n x b) = et2 n b x

data Leaf (A : Set) : Set where
  leaf : A → Leaf A

leafinj : {A : Set}(a b : A) → leaf a ≡ leaf b → a ≡ b
leafinj a b x = cong leafpred x where
  leafpred : {A : Set} → Leaf A → A
  leafpred (leaf a) = a