module lec01 where

open import Lib hiding (id; _∘_)

{-
admin:

17.45--19.15

Kaposi Ambrus, akaposi@inf.elte.hu, Programozási Nyelvek és Fordítóprogramok Tanszék
ti is tegezzetek
- bármikor közbe lehet szólni
- bitbucket.org/akaposi/ttt  <- minden infó itt van
- technikai kovetelmeny: Agda, Emacs

Lean
-}

{-
- related courses:
  - funkcionalis programozas
    - Agda = Haskell + totalitas(strong) + kifejezobb tipusok (fuggo tipusok)
      (List Bool n)(Mat m n)(OrderedList)
      sort : List Int -> List Int
      sort "hello" ezt visszautasitja a tipusok alapjan a forditprogram
      sort xs = []
      sort : List Int n -> List Int n
      sort xs = xs
      sort : List Int n -> OrderedList Int n
      sort xs = [1,2,3,...,n]
      sort : (xs : List Int n) -> (ys : OrderedList Int n) × ys isPerm xs
      sort xs = ([1,2,3,...,n], vegtelen ciklus)
      sort xs = sort xs

      add : Int -> Int -> Int
      add x y | x == 864298652987259 && y == 79284165812 = 42
              | otherwise = x + y
    
  - logika
  - diszkret matek (algebra)
  - szamitaselmelet
- CS alkalmazasok:
  - Google Chrome: SSL library BoringSSL
  - Linux kernel
  - CompCert
  - smart contract, zero knowledge proofs
- matematikai alkalmazasok:
    matematikai allitas        <--->     tipus
    bizonyitas                 <--->     program
  - Curry-Howard izomorfizmus
  - hosszu, megbizhatatlan a review folyamat
  - Coq, Lean
- implementations
  Agda  sved      tipuselmelet-kutatok hasznaljak, legtobb feature, Haskell
  Coq   francia   leginkabb hasznalt, matek is es programozas is, OCaml
  Idris skot      programozas, Haskell, Idris
  Lean  amerikai  matematikusok, C
-}

add2 : ℕ → ℕ       -- \bN
add2 x = x + 2

plus : ℕ → (ℕ → ℕ)
plus x y = x + y

k k' : (ℕ → ℕ) → ℕ
k h = h 2 + h 3
k' = λ h → h 2 + h 3

-- k add2 = add2 2 + add2 3 = (2 + 2) + (3 + 2) = 4 + 5 = 9

add2' add2'' : ℕ → ℕ
add2'  = _+ 2      -- = (λ x → x + 2)      (+2)
add2'' = 2 +_      -- = (λ x → 2 + x)      (2+)
add' = _+_

-- (λ x → t) u = t[x↦u] = t-ben az x osszes elofordulasat u-ra csereljuk (β-redukcio)
{-
   k add2 =
   (λ h → h 2 + h 3) (λ x → x + 2) =
--        \_______/  \_________/
-- (λ x →     t    )       u
-- t[x↦u]
   (h 2 + h 3)[h↦(λ x → x + 2)] =
   ((λ x → x + 2) 2 + (λ x → x + 2) 3) =
--  (λ x → t    ) u
   ((x+2)[x↦2]) + (x+2)[x↦3] =
   (2+2) + (3+2) =
   4 + 5 =
   9
-}

-- id :: a -> a
id : {A : Set} → A → A
id = λ x → x

-- id 4 = (λ x → x) 4 = x[x↦4] = 4

_∘_ : {A B C : Set} → (B → A) → (C → B) → C → A
(f ∘ g) x = f (g x)

square : ℕ → ℕ
square x = x * x

-- (add2 ∘ square) 1 = (((λ f → (λ g → (λ x → f (g x)))) add2) square) 1 = ...
-- add2 (square 1) = 3

-- (square ∘ add2) 1 = square 3 = 9

-- _+_

-- kovetkezo ora 2 perccel rovidebb
