module gy06 where

open import Lib hiding (K)

----------------------------------------------
-- Some Sigma types
----------------------------------------------

Σ=⊎ : {A B : Set} → Σ Bool (if_then A else B) ↔ A ⊎ B
Σ=⊎ = {!!}

Σ=× : {A B : Set} → Σ A (λ _ → B) ↔ A × B
Σ=× = {!!}

-- Π A F is essentially (a : A) → F a
-- what does this mean?

                    -- Π A (λ _ → B)
Π=→ : {A B : Set} → ((a : A) → (λ _ → B) a) ≡ (A → B)
Π=→ = {!!}

                    -- Π Bool (if_then A else B)
→=× : {A B : Set} → ((b : Bool) → if b then A else B) ↔ A × B
→=× = {!!}

dependentCurry : {A : Set}{B : A → Set}{C : (a : A) → B a → Set} →
  ((a : A)(b : B a) → C a b) ↔ ((w : Σ A B) → C (fst w) (snd w))
dependentCurry = {!!}

---------------------------------------------------------
-- propositional logic
------------------------------------------------------

-- Curry-Howard izomorfizmus
-- Elmélet:
--   ∙ átalakítani logikai állításokat típusokra.
--   ∙ formalizálni állításokat típusokkal.
--   × = ∧ = konjunkció
--   ⊎ = ∨ = diszjunkció
--   ¬ = ¬ = negáció
--   ⊃ = → = implikáció

--------------------------------------------------
-- Formalisation
--------------------------------------------------

-- Formalizáljuk a mondatokat!

-- Az egyes formalizált alap mondatrészeket vegyük fel modul paraméterként, akkor szépen fog működni minden.
module Formalise where

  -- Nem süt a nap.
  form1 : Set
  form1 = ?

  -- Esik az eső és süt a nap.
  form2 : Set
  form2 = ?

  -- Nem kell az esernyő vagy esik az eső.
  form3 : Set
  form3 = ?

  -- Ha esik az eső és süt a nap, akkor van szivárvány.
  form4 : Set
  form4 = ?

  -- Van szivárvány.
  K : Set
  K = ?

---- Következményfogalom (logika tárgy 1-3. gyakorlat)
  -- Agdában legegyszerűbben szintaktikus következményekkel lehet foglalkozni.

  -- Mondd ki, és bizonyítsd be, hogy a fenti állításokból következik a K.
  -- A típusban kell kimondani az állítást; az állítás kimondásához az eldöntésprobléma tételét kell használni.
  -- Két féleképpen lehet bizonyítani.

  Köv : Set
  Köv = ?

  Köv1 : Köv
  Köv1 = ?

  Köv2 : Köv
  Köv2 = ?

----------------------------------------------------------------------------

subt-prod : {A A' B B' : Set} → (A → A') → (B → B') → A × B → A' × B'
subt-prod = {!!}

subt-fun : {A A' B B' : Set} → (A → A') → (B → B') → (A' → B) → (A → B')
subt-fun = {!!}

anything : {X Y : Set} → ¬ X → X → Y
anything = {!!}

ret : {X : Set} → X → ¬ ¬ X
ret = {!!}

fun : {X Y : Set} → (¬ X) ⊎ Y → (X → Y)
fun = {!!}

-- De Morgan

dm1 : {X Y : Set} →  ¬ (X ⊎ Y) ↔ ¬ X × ¬ Y
dm1 = {!!}

dm2 : {X Y : Set} → ¬ X ⊎ ¬ Y → ¬ (X × Y)
dm2 = {!!}

dm2b : {X Y : Set} → ¬ ¬ (¬ (X × Y) → ¬ X ⊎ ¬ Y)
dm2b = {!!}

-- stuff

nocontra : {X : Set} → ¬ (X ↔ ¬ X)
nocontra = {!!}

¬¬invol₁ : {X : Set} → ¬ ¬ ¬ ¬ X ↔ ¬ ¬ X
¬¬invol₁ = {!!}

¬¬invol₂ : {X : Set} → ¬ ¬ ¬ X ↔ ¬ X
¬¬invol₂ = {!!}

nnlem : {X : Set} → ¬ ¬ (X ⊎ ¬ X)
nnlem = {!!}

nndnp : {X : Set} → ¬ ¬ (¬ ¬ X → X)
nndnp = {!!}

dec2stab : {X : Set} → (X ⊎ ¬ X) → (¬ ¬ X → X)
dec2stab = {!!}

-- you have to decide:
{-
Dec : Set → Set
Dec A = A ⊎ ¬ A
-}

open import Lib.Dec.PatternSynonym

ee1 : {X Y : Set} → Dec (X ⊎ Y → ¬ ¬ (Y ⊎ X))
ee1 = {!!}

ee2 : {X : Set} → Dec (¬ (X ⊎ ¬ X))
ee2 = {!!}

e3 : {X : Set} → Dec (¬ (X → (¬ X → X)))
e3 = {!!}

e4 : Dec ℕ
e4 = {!!}

e5 : Dec ⊥
e5 = {!!}

e6 : {X : Set} → Dec (⊥ → X ⊎ ¬ X)
e6 = {!!}

e7 : {X : Set} → Dec (X × ¬ X → ¬ X ⊎ X)
e7 = {!!}

e8 : {X : Set} → Dec ((X → X) → ⊥)
e8 = {!!}

f1 : {X Y : Set} → ¬ ¬ X ⊎ ¬ ¬ Y → ¬ ¬ (X ⊎ Y)
f1 = {!!}

f2 : ({X Y : Set} → ¬ (X × Y) → ¬ X ⊎ ¬ Y) → {X Y : Set} → ¬ ¬ (X ⊎ Y) → ¬ ¬ X ⊎ ¬ ¬ Y
f2 = {!!}

----------------------------------------------------------------------
-- Not exactly first order logic but kinda is and kinda isn't.

f3 : Dec ((X Y : Set) → X ⊎ Y → Y)
f3 = {!!}

f4 : Dec ((X Y Z : Set) → (X → Z) ⊎ (Y → Z) → (X ⊎ Y → Z))
f4 = {!!}

f5 : Dec ((X Y Z : Set) → (X → Z) × (Y → Z) → (X × Y → Z))
f5 = {!!}

f6 : Dec ((X Y Z : Set) → (X × Y → Z) → (X → Z) × (Y → Z))
f6 = {!!}

f7 : Dec ((X Y Z : Set) → (X ⊎ Y × Z) → (X ⊎ Y) × (X ⊎ Z))
f7 = {!!}

f8 : Dec ((X Y Z : Set) → (X ⊎ Y) × (X ⊎ Z) → ((X ⊎ Y) × Z))
f8 = {!!}

f9 : Dec ((X Y Z : Set) → (X ⊎ Y) × (X ⊎ Z) → (X ⊎ Y × Z))
f9 = {!!}
