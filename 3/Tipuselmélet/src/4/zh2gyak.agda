module zh2gyak where

open import Lib

transpose : {A : Set}{n k : ℕ} → Vec (Vec A n) k → Vec (Vec A k) n
transpose {A} {zero} {zero} x = x
transpose {A} {zero} {suc k} x = []
transpose {A} {suc n} {zero} x = [] ∷ (transpose {A} {n} {0} [])
transpose {A} {suc n} {suc k} ((a ∷ as) ∷ tomb) = (a ∷ (Vec.map Vec.head tomb)) ∷ transpose (as ∷ (Vec.map Vec.tail tomb))

hat : {A B : Set} → Dec (¬ ((A ⊎ B → A) ⊎ (A ⊎ B → B)))
hat = inr λ x → x (inr (λ a⊎b → case a⊎b (λ a → exfalso (x (inl (λ _ → a)))) id))

heta : Dec ({A : Set}(P : A → Set)(f : A → A) → ((x : A) → P x) → ((x : A) → P (f x)))
heta = inl (λ P f g a → g (f a))

hetb : Dec ((A : Set)(P : A → Set)(f : A → A) → ((x : A) → P (f x)) → ((x : A) → P x))
hetb = inr (λ x → x A P f (λ _ → tt) false) where
    A : Set
    A = Bool
    P : Bool → Set
    P true = ⊤
    P false = ⊥
    f : Bool → Bool
    f _ = true

nyolc : (n : ℕ) → suc (n * (suc (suc n))) ≡ (n + suc zero) ^ (suc (suc zero)) + zero
nyolc n = sym 
    (trans (idr+ ((n + 1) * ((n + 1) * 1))) 
    (trans (cong (λ m → m * ((n + 1) * 1)) (comm+ n 1)) 
    (trans (cong (λ m → m + n * m) (idr* (n + 1))) 
    (trans (cong (λ m → m + n * m)  (comm+ n 1))
    (cong suc 
    (trans (cong (λ m → n + m)  (comm* n (suc n))) 
    (sym (comm* n (suc (suc n))))) )))))

alma : Vec Bool 0
alma = []

data Vec' : (A : Set) → ℕ → Set₁ where
    nil' : (A : Set) → Vec' A 0
    cons : (A : Set)(n : ℕ) → A → Vec' A n → Vec' A (suc n)
 
iteVec' : {A : Set}{C : (A : Set) → ℕ → Set₁}{n : ℕ} → ((A : Set) → C A 0) → ((A : Set)(n : ℕ) → A → C A n → C A (suc n)) → Vec' A n → C A n
iteVec' n c (nil' A) = n A 
iteVec' n c (cons A m a v) = c A m a (iteVec' n c v)


data Vec'' (A : Set) : ℕ → Set where
  nil : Vec'' A 0
  cons : (n : ℕ) → A → Vec'' A n → Vec'' A (suc n)


iteVec'' : {A : Set}{C : ℕ → Set}(nb : C 0)(cb : (n : ℕ) → A → C n → C (suc n))(n : ℕ) → Vec'' A n → C n
iteVec'' nb cb .0 nil = nb 
iteVec'' nb cb .(suc n) (cons n x xs) = cb n x (iteVec'' nb cb n xs)

ηVec'' : {A : Set}{C : ℕ → Set}(nb : C 0)(cb : (n : ℕ) → A → C n → C (suc n)) → 
        (f : (n : ℕ) →  Vec'' A n → C n) →
        (f 0 nil ≡ nb) →
        ((n : ℕ)(x : A)(xs : Vec'' A n) → f (suc n) (cons n x xs) ≡ cb n x (f n xs)) → 
        (n : ℕ)(xs : Vec'' A n) → f n xs ≡ iteVec'' nb cb n xs
ηVec'' nb cb f en ec .0 nil = en 
ηVec'' nb cb f en ec .(suc n) (cons n x xs) = trans (ec n x xs) (cong (cb n x) (ηVec'' nb cb f en ec n xs))

data X : ℕ → Set where
    X1 : (n k : ℕ) → X n → X (n + k)
    X2 : (n : ℕ) → Fin n → X (suc n)

iteX : {C : ℕ → Set}(x1 : (n k : ℕ) → C n → C (n + k))(x2 : (n : ℕ) → Fin n → C (suc n))(n : ℕ) → X n → C n
iteX x1 x2 .(n + k) (X1 n k x) = x1 n k (iteX x1 x2 n x)    --β₁
iteX x1 x2 .(suc n) (X2 n x) = x2 n x                       --β₂

ηX : {C : ℕ → Set}(x1 : (n k : ℕ) → C n → C (n + k))(x2 : (n : ℕ) → Fin n → C (suc n)) →
    (f : (n : ℕ) → X n → C n) →
    (ex1 : (n k : ℕ)(x : X n) → f (n + k) (X1 n k x) ≡ x1 n k (f n x)) →
    (ex2 : (n : ℕ)(x : Fin n) → f (suc n) (X2 n x) ≡ x2 n x) →
    (n : ℕ)(x : X n) → f n x ≡ iteX x1 x2 n x
ηX x1 x2 f ex1 ex2 .(n + k) (X1 n k x) = trans (ex1 n k x) (cong (x1 n k) (ηX x1 x2 f ex1 ex2 n x))
ηX x1 x2 f ex1 ex2 .(suc n) (X2 n x) = ex2 n x              --η