-- BEGIN FIX
{-# OPTIONS --guardedness --safe #-}

open import Lib.Level
open import Lib.Function
open import Lib.Nat
  renaming (_≟_ to _≟ℕ_)
open import Lib.Empty
  renaming (_≟_ to _≟⊥_)
open import Lib.Unit
  renaming (_≟_ to _≟⊤_)
open import Lib.Equality
open import Lib.Sum
  renaming (map to map⊎)
open import Lib.Sigma
  renaming (map to mapΣ)
open import Lib.Containers.Stream.Type
open import Lib.Containers.Vector.Type
open import Lib.Fin
open import Lib.Bool
  renaming ( contradiction to contradictionᵇ
           ; contraposition to contrapositionᵇ
           ; _≟_ to _≟ᵇ_)
open import Lib.Dec
open import Lib.Conat.Type
open import Lib.Conat.Base
  renaming (_+_ to _+∞_ ; _*_ to _*∞_ ; _^_ to _^∞_)
open import Lib.Conat.Literals
open import Lib.Maybe
open import Lib.Relation
open import Lib.Class.Eq
open import Lib.Class.Ord
import Lib.Containers.Stream as S
import Lib.Containers.Vector as V

_≤_ : ℕ → ℕ → Set
n ≤ k = Σ ℕ λ m → m + n ≡ k
infix 5 _≤_
-- END FIX

-- BEGIN FIX
-- b1 and b2 should be such that b1 ℕ 1 2 ≠ b2 ℕ 1 2
b1 b2 : (A : Set) → A → A → A
-- END FIX
b1 A a b = a
b2 A a b = b
-- BEGIN FIX
test-b1-b2 : ¬ (b1 ℕ 1 2 ≡ b2 ℕ 1 2)
test-b1-b2 ()
-- END FIX

-- BEGIN FIX
weirdLogicalEquiv : (A B C : Set) → (B → A → (⊥ ⊎ C)) ↔ (A → (B → C × A))
-- END FIX
weirdLogicalEquiv A B C = (λ f a b → case (f b a) exfalso (λ c → c) , a) , (λ f b a → inr (fst (f a b)))

-- BEGIN FIX
cocΣ : (A : Set)(B : A → Set) → Σ A B ↔ ((C : Set) → ((a : A) → B a → C) → C)
-- END FIX
fst (cocΣ A B) (a , ba) C f = f a ba
snd (cocΣ A B) f = f (Σ A B) (λ a ba → a , ba)

-- BEGIN FIX
prop : {P : Set} → P ⊎ ¬ P → (¬ ( ¬ P) → P)
-- END FIX
prop = λ x x₁ → case x (λ z → z) λ x₂ → exfalso (x₁ x₂)

-- BEGIN FIX
ref≤ : Reflexive _≤_
-- END FIX
ref≤ = zero , refl

-- BEGIN FIX
cong⁻¹ : {A B : Set}(a b : A)(f : A → B) → ¬ (f a ≡ f b) → ¬ (a ≡ b)
-- END FIX
cong⁻¹ a b f ¬f a≡b = ¬f (cong f a≡b)

-- BEGIN FIX
a+b=0→a=0 : (a b : ℕ) → (a + b) ≡ 0 → a ≡ 0
-- END FIX
a+b=0→a=0 zero b e = refl
a+b=0→a=0 (suc a) b ()

-- BEGIN FIX
noℕsqrt : ¬ ((n k : ℕ) → Σ ℕ λ m → m * m ≡ n * k)
-- END FIX
noℕsqrt x with x 1 2
... | suc (suc zero) , ()
... | suc (suc (suc m)) , pr with cong (λ x → pred' (pred' x)) pr
... | ()

-- BEGIN FIX
¬¬∃↓ : ¬ ((f : ℕ → ℕ) → Σ ℕ λ n → (k : ℕ) → suc (f n) ≤ (f k))
-- END FIX
¬¬∃↓ x with x (λ _ → 0)
... | num , proof with proof 0
... | zero , ()
... | suc num₂ , ()

-- BEGIN FIX
-- works like haskell's zip
zipStream : {A B : Set} → Stream A → Stream B → Stream (A × B)
-- END FIX
head (zipStream sa sb) = (head sa) , (head sb)
tail (zipStream sa sb) = zipStream (tail sa) (tail sb)
-- BEGIN FIX
test-s1 : S.take 10 (zipStream (S.iterate suc 0) (S.iterate pred' 100)) ≡
  (0 , 100) ∷ (1 , 99) ∷ (2 , 98) ∷
  (3 , 97)  ∷ (4 , 96) ∷ (5 , 95) ∷
  (6 , 94)  ∷ (7 , 93) ∷ (8 , 92) ∷
  (9 , 91) ∷ V.[]
test-s1 = refl
test-s2 : S.take 10 (S.map (λ (a , b) → a + b) (zipStream (S.iterate (λ x → suc (suc x)) 0) (S.iterate pred' 100))) ≡
  100 ∷ 101 ∷ 102 ∷ 103 ∷ 104 ∷ 105 ∷ 106 ∷ 107 ∷ 108 ∷ 109 ∷ V.[]
test-s2 = refl
-- END FIX

