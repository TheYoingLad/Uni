module gy10 where

open import Lib hiding (_≟ℕ_)
open List

---------------------------------------------------------
-- konstruktorok injektivitasa
------------------------------------------------------

sucinj : (m n : ℕ) → suc m ≡ suc n → m ≡ n
sucinj m .m refl = refl

-- prove it without pattern matching on e! (hint: use pred)
sucinj' : (m n : ℕ) → suc m ≡ suc n → m ≡ n
sucinj' m n e = cong pred' e

data Tree : Set where
  leaf : Tree
  node : (ℕ → Tree) → Tree

nodeinj : ∀{f g} → node f ≡ node g → f ≡ g
nodeinj {f} {g} e = subst (λ x → f ≡ remnode x) e refl where
    remnode : Tree → (ℕ → Tree)
    remnode (node f) = f
    remnode _ _ = leaf

data BinTree : Set where
  leaf : BinTree
  node : BinTree → BinTree → BinTree

nodePredL : BinTree → BinTree
nodePredL (node l _) = l
nodePredL _ = leaf

nodePredR : BinTree → BinTree
nodePredR (node _ r) = r
nodePredR _ = leaf

nodeinjl : ∀{x y x' y'} → BinTree.node x y ≡ node x' y' → x ≡ x'
nodeinjl {x} {y} {x'} {y'} e = subst (λ t → x ≡ nodePredL t) e refl

nodeinjr : ∀{x y x' y'} → BinTree.node x y ≡ node x' y' → y ≡ y'
nodeinjr {x} {y} {x'} {y'} e = subst (λ t → y ≡ nodePredR t) e refl

predL∷ : {A : Set} → List A → A
predL∷ (x ∷ _) = x
predL∷ _ = _

predR∷ : {A : Set} → List A → List A
predR∷ (_ ∷ xs) = xs
predR∷ _ = []

∷inj1 : {A : Set}{x y : A}{xs ys : List A} → x ∷ xs ≡ y ∷ ys → x ≡ y
∷inj1 {A} {x} {y} {xs} {ys} e = subst (λ l →  x ≡ predL∷ l) e refl

∷inj2 : {A : Set}{x y : A}{xs ys : List A} → x ∷ xs ≡ y ∷ ys → xs ≡ ys
∷inj2 {A} {x} {y} {xs} {ys} e = subst (λ l → xs ≡ predR∷ l) e refl

-- prove all of the above without pattern matching on equalities!

---------------------------------------------------------
-- konstruktorok diszjunktsaga
------------------------------------------------------

true≠false : true ≢ false
true≠false ()

-- prove this without pattern matching in this function on e! (use subst!)
true≠false' : true ≢ false
true≠false' e = subst (λ x → if x then ⊤ else ⊥) e tt

zero≠sucn : {n : ℕ} → zero ≢ suc n
zero≠sucn e = subst (λ x → P x) e tt where
    P : ℕ → Set
    P zero = ⊤
    P _ = ⊥

n≠sucn : (n : ℕ) → n ≢ suc n
n≠sucn n ()

-- prove this using induction on n!
n≠sucn' : (n : ℕ) → n ≢ suc n
n≠sucn' zero ()
n≠sucn' (suc n) e = n≠sucn' n (cong pred' e)

leaf≠node : ∀{f} → Tree.leaf ≢ node f
leaf≠node e = subst (λ t → P t) e tt where
    P : Tree → Set
    P leaf = ⊤
    P _ = ⊥

leaf≠node' : ∀{x y} → BinTree.leaf ≢ node x y
leaf≠node' e = subst (λ t → P t) e tt where
    P : BinTree → Set
    P leaf = ⊤
    P _ = ⊥

nil≠cons : {A : Set}{x : A}{xs : List A} → [] ≢ x ∷ xs
nil≠cons e = subst (λ t → P t) e tt where
    P : {A : Set} → List A → Set
    P [] = ⊤
    P _ = ⊥

---------------------------------------------------------
-- rendezes
------------------------------------------------------

_≤_ : ℕ → ℕ → Set
x ≤ y = Σ ℕ λ m → m + x ≡ y

1≤3 : 1 ≤ 3
1≤3 = 2 , refl

¬2≤1 : ¬ (2 ≤ 1)
¬2≤1 (suc zero , ())
¬2≤1 (suc (suc k) , ())

n≤sucn : ∀ (n : ℕ) → n ≤ suc n
n≤sucn n = 1 , refl

suc-monotonous≤ : ∀ (n m : ℕ) → n ≤ m → suc n ≤ suc m
suc-monotonous≤ n m (k , e) = k , trans (sucr+ k n) (cong suc e)

sucinj≤ : ∀ (n m : ℕ) → suc n ≤ suc m → n ≤ m
sucinj≤ n m (k , e) = k , cong pred' suck+n=sucm where
    suck+n=sucm : suc (k + n) ≡ suc m
    suck+n=sucm = trans (sym $ sucr+ k n) e

---------------------------------------------------------
-- egyenlosegek eldonthetosege
------------------------------------------------------

_≟Bool_ : (b b' : Bool) → Dec (b ≡ b')
false ≟Bool false = inl refl
false ≟Bool true = inr (λ ())
true ≟Bool false = inr (λ ())
true ≟Bool true = inl refl

_≟ℕ_ : (n n' : ℕ) → Dec (n ≡ n')
zero ≟ℕ zero = inl refl
zero ≟ℕ suc n' = inr (λ ())
suc n ≟ℕ zero = inr (λ ())
suc n ≟ℕ suc n' with n ≟ℕ n'
... | inl refl = inl refl
... | inr ne = inr λ f → ne (cong pred' f)


-- is equality for Tree decidable?

_≟BinTree_ : (t t' : BinTree) → Dec (t ≡ t')
leaf ≟BinTree leaf = inl refl
leaf ≟BinTree node t t' = inr (λ ())
node t₁ t₂ ≟BinTree leaf = inr (λ ())
node t₁ t₂ ≟BinTree node t₁' t₂' with (t₁ ≟BinTree t₁') , (t₂ ≟BinTree t₂')
... | inl refl , inl refl = inl refl
... | inl refl , inr ne = inr (λ {refl → ne refl})
... | inr ne , inl e = inr (λ {refl → ne refl})
... | inr ne , inr ne' = inr (λ {refl → ne refl})

_≟List_ : {A : Set} → ({x y : A} → Dec (x ≡ y)) → {xs ys : List A} → Dec (xs ≡ ys)
_≟List_ {A} e {[]} {[]} = inl refl 
_≟List_ {A} e {[]} {y ∷ ys} = inr (λ ())
_≟List_ {A} e {x ∷ xs} {[]} = inr (λ ())
_≟List_ {A} e {x ∷ xs} {y ∷ ys} with (e {x} {y}) , (_≟List_ e {xs} {ys})
... | inl refl , inl refl = inl refl
... | inl refl , inr nes = inr λ {refl → nes refl}
... | inr ne , inl refl = inr λ {refl → ne refl}
... | inr ne , inr nes = inr λ {refl → ne refl}