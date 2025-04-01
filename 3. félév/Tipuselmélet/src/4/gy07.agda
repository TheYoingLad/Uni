module gy07 where

open import Lib

------------------------------------------------------
-- statements as parameters
------------------------------------------------------

blowUp : ((A : Set) → ¬ A) → ⊥ -- Nem igaz, hogy minden állítás hamis ⇒ Igaz
blowUp f = f ⊤ tt
-- what's the difference with this?
blowUp' : (A : Set) → ¬ A → ⊥ -- Minden állításra nem igaz, hogy nem igaz az állítás → Minden állítás igaz ⇒ Hamis
blowUp' A x = x {!   !}

-- something like this may appear in the exam

---------------------------------------------------------
-- predicate (first order) logic example
---------------------------------------------------------

notExists↔noneOf : ∀{i}{A : Set i} → (P : A → Set) →
                        (∀ x → ¬ (P x)) ↔ ¬ (Σ A (λ x → P x))
fst (notExists↔noneOf P) f (a , pa) = f a pa
snd (notExists↔noneOf P) f a pa = f (a , pa)

module People
  (Person    : Set)
  (Ann       : Person)
  (Kate      : Person)
  (Peter     : Person)
  (_childOf_ : Person → Person → Set)
  (_sameAs_  : Person → Person → Set) -- ez most itt az emberek egyenlosege
  where

  -- Define the _hasChild predicate.
  _hasChild : Person → Set
  x hasChild = Σ Person λ y → y childOf x

  -- Formalise: Ann is not a child of Kate.
  ANK : Set
  ANK = ¬ (Ann childOf Kate)

  -- Formalise: there is someone with exactly one child.
  ONE : Set
  ONE = Σ Person λ x → Σ Person λ z → (z childOf x) × (∀ y → ¬ (y sameAs z) → ¬ (y childOf x))

  -- Define the relation _parentOf_.
  _parentOf_ : Person → Person → Set
  x parentOf y = y childOf x

  -- Formalise: No one is the parent of everyone.
  NOPE : Set
  NOPE = ¬ (Σ Person λ x → ∀ y → x parentOf y)

  -- Prove that if Ann has no children then Kate is not the child of Ann.
  AK : ¬ (Σ Person λ y → y childOf Ann) → ¬ (Kate childOf Ann)
  AK f kca = f (Kate , kca)

  -- Prove that if there is no person who is his own parent than no one is the parent of everyone.
  ¬xpopxthenNOPE : ¬ (Σ Person λ x → x parentOf x) → NOPE
  ¬xpopxthenNOPE f (p , pb) = f (p , pb p)

---------------------------------------------------------
-- predicate (first order) logic laws
---------------------------------------------------------

∀×-distr  :    (A : Set)(P : A → Set)(Q : A → Set) → ((a : A) → P a × Q a)  ↔ ((a : A) → P a) × ((a : A) → Q a)
fst (fst (∀×-distr A P Q) x) = λ a → fst (x a)
snd (fst (∀×-distr A P Q) x) = λ a → snd (x a)
snd (∀×-distr A P Q) (pa , qa) a = (pa a) , (qa a)

∀⊎-distr  :    (A : Set)(P : A → Set)(Q : A → Set) → ((a : A) → P a) ⊎ ((a : A) → Q a) → ((a : A) → P a ⊎ Q a)
∀⊎-distr A P Q x a = case x (λ pa → inl (pa a)) (λ qa → inr (qa a))
-- ez miért csak odafelé megy?
-- miért nem ↔ van közte?

Σ×-distr  :    (A : Set)(P : A → Set)(Q : A → Set) → (Σ A λ a → P a × Q a)  → Σ A P × Σ A Q
Σ×-distr A P Q (a , (pa , qa)) = (a , pa) , (a , qa)

Σ⊎-distr  :    (A : Set)(P : A → Set)(Q : A → Set) → (Σ A λ a → P a ⊎ Q a)  ↔ Σ A P ⊎ Σ A Q
fst (Σ⊎-distr A P Q) (a , f) = case f (λ pa → inl (a , pa)) (λ qa → inr (a , qa))
snd (Σ⊎-distr A P Q) (inl (a , pa)) = a , (inl pa)
snd (Σ⊎-distr A P Q) (inr (a , qa)) = a , (inr qa)

¬∀        :    (A : Set)(P : A → Set)              → (Σ A λ a → ¬ P a)      → ¬ ((a : A) → P a)
¬∀ A P (a , ¬pa) f = ¬pa (f a)

-- Ugyanez van a fájl tetején is:
¬Σ        :    (A : Set)(P : A → Set)              → (¬ Σ A λ a → P a)      ↔ ((a : A) → ¬ P a)
fst (¬Σ A P) f a pa = f (a , pa)
snd (¬Σ A P) f (a , pa) = f a pa

¬¬∀-nat   :    (A : Set)(P : A → Set)              → ¬ ¬ ((x : A) → P x)    → (x : A) → ¬ ¬ (P x)
¬¬∀-nat A P f a ¬pa = f (λ g → ¬pa (g a))

∀⊎-distr' : ¬ ((A : Set)(P : A → Set)(Q : A → Set) → (((a : A) → P a ⊎ Q a) → ((a : A) → P a) ⊎ ((a : A) → Q a)))
∀⊎-distr' x = cont counterExample where
  P : Bool → Set
  P true = ⊤
  P false = ⊥

  Q : Bool → Set
  Q true = ⊥
  Q false = ⊤
  
  p : (a : Bool) → P a ⊎ Q a
  p true = inl tt
  p false = inr tt
  
  counterExample : ((a : Bool) → P a) ⊎ ((a : Bool) → Q a)
  counterExample = x Bool P Q p

  cont : ¬ (((a : Bool) → P a) ⊎ ((a : Bool) → Q a))
  cont (inl p) = p false
  cont (inr q) = q true

Σ×-distr' : ¬ ((A : Set)(P : A → Set)(Q : A → Set) → (Σ A P × Σ A Q → Σ A λ a → P a × Q a))
Σ×-distr' x = cont counterExample where
  P : Bool → Set
  P true = ⊤
  P false = ⊥

  Q : Bool → Set
  Q true = ⊥
  Q false = ⊤

  p : Σ Bool P × Σ Bool Q  
  p = (true , tt) , (false , tt)

  counterExample : Σ Bool λ a → P a × Q a
  counterExample = x Bool P Q p

  cont : ¬ (Σ Bool λ a → P a × Q a)
  cont (true , x) = snd x
 
Σ∀       : (A B : Set)(R : A → B → Set)        → (Σ A λ x → (y : B) → R x y) → (y : B) → Σ A λ x → R x y
Σ∀ A B R (a , f) b = a , (f b)
AC       : (A B : Set)(R : A → B → Set)        → ((x : A) → Σ B λ y → R x y) → Σ (A → B) λ f → (x : A) → R x (f x)
AC A B R f = (λ a → fst (f a)) , (λ a → snd (f a)) 