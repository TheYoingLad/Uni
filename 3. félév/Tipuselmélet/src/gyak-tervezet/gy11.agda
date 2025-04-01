module gy11 where

-- Proofs on other types (we can have proofs on other types as well, not just ℕ)

open import Lib

List-idr++ : ∀{i}{A : Set i}(xs : List A) → let open List in xs ++ [] ≡ xs
List-idr++ xs = {!!}

Vec-idr++ : ∀{i}{A : Set i}{n}(xs : Vec A n) → let open Vec in xs ++ [] ≡ subst (Vec A) (sym (idr+ n)) xs
Vec-idr++ xs = {!!}

List-dist-length++ : ∀{i}{A : Set i}(xs ys : List A) → let open List in length (xs ++ ys) ≡ {!!}
List-dist-length++ = {!!}

-- List ++ is associative! State it and then prove it!
List-assoc++ : {!!}
List-assoc++ = {!!}

-- Vec ++ associative! (This is hard to state and very difficult to prove!)
Vec-assoc++ : {!!}
Vec-assoc++ = {!!}

-- Associativity is needed here
reverse++-dist : ∀{i}{A : Set i}{xs ys : List A} → let open List in reverseNaive (xs ++ ys) ≡ {!!}
reverse++-dist = {!!}

------------------------------------------
-- How to model mathematical structures?
------------------------------------------

-- Different formalisations are possible.
-- e.g. we can encode the semigroup structure inside the record, not in the type.
-- Every form has its pros and cons.
-- In this formalisation the type and the operation is known at type level, we need to prove that the operation is associative.

-- State the associativity rule.
record Semigroup {i}(A : Set i)(_⊕_ : A → A → A) : Set i where
  field
    assoc : {!!}

open Semigroup {{...}} public

-- Something to be a monoid has to be Semigroup.
-- State the identity rules.
record Monoid {i}(A : Set i)(_⊕_ : A → A → A)(e : A) : Set i where
  field
    overlap ⦃ semigroup ⦄ : {!!}
    idl : {!!}
    idr : {!!}

open Monoid {{...}} public

instance
-- Formalise the followings:
---- Natural numbers with _+_ form a semigroup
  ℕ+Semigroup : {!!}
  ℕ+Semigroup = {!!}

---- Natural numbers with _*_ form a semigroup
  ℕ*Semigroup : {!!}
  ℕ*Semigroup = {!!}

---- What can be the associative operation on lists?
  ListSemigroup : {!!}
  ListSemigroup = {!!}

-- The above ones also form a monoid.
  ℕ+Monoid : {!!}
  ℕ+Monoid = {!!}

  ℕ*Monoid : {!!}
  ℕ*Monoid = {!!}

  ListMonoid : {!!}
  ListMonoid = {!!}

-----------------------------
-- Real isomorphism
-----------------------------

-- We can define a type for actual isomorphism.
-- The ↔ type is just logical equivalence but it does not require it to be an isomorphism.
-- Isomorphism requires us to prove that the given two functions in ↔ composed in any order is the identity function.

-- \~= : ≅
-- infix 0 _≅_
-- record _≅_ {i j}(A : Set i)(B : Set j) : Set (i ⊔ j) where
--   constructor ≅-proof
--   field
--     coe→ : A → B
--     coe← : B → A
--     id←→ : ∀ a → coe← (coe→ a) ≡ a -- funexted version
--     id→← : ∀ b → coe→ (coe← b) ≡ b

-- open _≅_ public

-- e.g.

Bool≅Fin2 : Bool ≅ Fin {!!} -- Fill in the above type holes in the instance block to be able to write the 2 literal here. (suc (suc zero) still works, just literals don't.)
Bool≅Fin2 = {!!}

-- Let's remember some of the equivalences from earlier excercise classes (2ⁿᵈ excersise class in particular).
{-
-- (⊎, ⊥) form a commutative monoid (kommutatív egységelemes félcsoport)

assoc⊎ : {A B C : Set} → (A ⊎ B) ⊎ C ↔ A ⊎ (B ⊎ C)
idl⊎ : {A : Set} → ⊥ ⊎ A ↔ A
idr⊎ : {A : Set} → A ⊎ ⊥ ↔ A
comm⊎ : {A B : Set} → A ⊎ B ↔ B ⊎ A

-- (×, ⊤) form a commutative monoid (kommutatív egységelemes félcsoport)

assoc× : {A B C : Set} → (A × B) × C ↔ A × (B × C)
idl× : {A : Set} → ⊤ × A ↔ A
idr× : {A : Set} → A × ⊤ ↔ A
null× : {A : Set} → A × ⊥ ↔ ⊥

-- distributivity of × and ⊎
dist : {A B C : Set} → A × (B ⊎ C) ↔ (A × B) ⊎ (A × C)

-- exponentiation laws
curry : ∀{A B C : Set} → (A × B → C) ↔ (A → B → C)
⊎×→ : {A B C D : Set} → ((A ⊎ B) → C) ↔ (A → C) × (B → C)

law^0 : {A : Set} → (⊥ → A) ↔ ⊤
law^1 : {A : Set} → (⊤ → A) ↔ A
law1^ : {A : Set} → (A → ⊤) ↔ ⊤
-}

-- Choose one or two (or even all of them, these are not hard to prove) and try to formalise and then prove that going in either way we get back the original value.
-- To state it, you just need to change the ↔ with ≅
