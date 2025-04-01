module gy05 where

open import Lib hiding (fromℕ; minMax; head; tail)
open import Lib.Containers.Vector hiding (head; tail; map; length; _++_)
open import Lib.Containers.List hiding (head; tail; map; _++_; filter)

-- Ma kevés anyag : Függő típusok, és Vektor, Fin
-- Ha marad idő óra végén visszamegyünk megnézni régebbi feladatokat

-- Gyakorlás ZH-ra, 1-4 órai feladatok
-- Plusz mintazh megoldós konzi : mikor?
-- Elméleti rész:
--   - Típusok elemszáma                  -- 2. óra
--   - Egyenlőségi érvelés : futtasd fejben a kódot, általában behelyettesítés
--   - data-ával megadott típus elemszáma -- most ránézünk
--   - data-val megadás                   -- most ránézünk
--   - konstruktor, β, η szabályok        -- előző óra
--   - strictly positivity                -- előző óra

-- Gyakorlatias feladatok:
--   - Haskell függvények csak Agdában
--   - iso-k (↔) algebrai típusokra (algebrai típus a data-val megadott dolgok, szoval kb minden amit eddig vettünk)
--   - Természetes számok feladat       |
--   - List, Tree, RoseTree,            | ezek ugye általánban rekurzívan
--   - Fentiek iterátorokkal, iteList, iteNat, ...

-- Ma : iterátorok, CoInd befejez, utánna függő típusok, sajnos gyakorlás nincs sok idő, ez mb

-- Kitérő : Implicit paraméter
{-

f : Bool → Bool   == f' : {Bool} → Bool
-}

f : ⊤ → Bool
f b = true
f' : {⊤} → Bool
f' = true

teszt : ⊤ → Bool
teszt b = f b 

-- Úgymond nem kell megadni a paraméreteket
teszt' : ⊤ → Bool
teszt' b = f' 

{-


-----------------------------------------------------------------------
| Típus | Hogyan adjuk meg elemeit | hogyan szedjük szét az elemeit   |
|       |      konstruktorok       |           destruktor             |
-----------------------------------------------------------------------
|   ⊤   |            tt            | pattern match, de úgy is egy van |
-----------------------------------------------------------------------
|   ⊥   |            --            |                      patternmatch|
-----------------------------------------------------------------------
| Bool  |       true, false        | if_then_else,        patternmatch|
-----------------------------------------------------------------------
| _×_   |   (_,_) , copatternmatch |  fst, snd ,          patternmatch|
-----------------------------------------------------------------------
| _⊎_   |        inl, inr          |     case,            patternmatch|
-----------------------------------------------------------------------
| Nat   |        zero, suc         |     iteNat,          patternmatch|
-----------------------------------------------------------------------
| List  |        _∷_, []           |    iteList (foldr)   patternmatch|
-----------------------------------------------------------------------
| Stream|        copattern match   |    head, tail                    |
-----------------------------------------------------------------------


megadva data-val, record-al:

data ⊤ : Set where
    tt : ⊤

data ⊥ : Set where

data Bool : Set where
    true : Bool
    false : Bool

data _⊎_ (A B : Set) where
    inl : A → _⊎_
    inr : B → _⊎_

record _×_ (A B : Set) : Set where
    field
        fst : A
        snd : B

data Nat : Set where
  zero : Nat
  suc : Nat → Nat

data List(A : Set) : Set where
  nil : List A
  cons : A → List A → List A

record Stream (A : Set) : Set where
  coinductive
  field
    head : A
    tail : Stream A
-}

{-

Két függő típus lesz: Σ, Π 

Dependent type = függő típus

Nagy ötlet! => Bool → Bool helyett, (b : Bool) → ?
Vagyis szeretném használni a bemenet b-t arra hogy megválasszam milyen típust adok vissza 

Példa:

Adatbázis, az adatbázisban egy oszlop amiből leakarok kérdezni lehet nullable

.........................
| Nev | Id | Szul-datum |
| VK  | 01 | 2000.01.01 |
| AN  | 02 |    Null    |
.........................

query : (c : Coll) → Ha Szul-datom akkor Maybe Date, Ha Id akkor szám, Ha Nev akkor String

Ez a Ha_,akkor_ rész egy általános függvény lesz nekünk, ami felhasználja a c-t

-- Általánosabban
Π-már kinda használtuk:

Π : (A : Set) → (B : A → Set) → ( (a : A) → B a )

mi ennek a típusa :

Π Bool (λ _ → Bool) : ? 

Megoldás : Bool → Bool

példa: Bool and IsTrue, Nat and IsNotZero or IsEven

A : Set
A = Bool

B : A → Set
B true  = ℕ
B false = ⊥ 

Π A B : (a : Bool) → (λ b → if b then ℕ else ⊥) a
Π A B true  = 0
Π A B false = ?

Π A B : (b : Bool) → IsTrue b
Π A B == λ b → if b then ? else ?

-- Nézzük meg
-}

-- Példa:

test : (a : Bool) → (λ b → if b then ℕ else ⊥) a
test false = {!   !} -- nemtudom megadni
test true = 0

IsTrue : Bool → Set
IsTrue true = ⊤
IsTrue false = ⊥

data IsNatZero : ℕ → Set where
  

data IsEven : ℕ → Set where

-- Pattern matching után csak a true ág marad mert ugye (IsTrue false)-nak nincs elem 
tesztF : (b : Bool) → IsTrue b → Bool
tesztF true a = true

testFteszt : Bool
testFteszt = tesztF true tt

{-

IsNotZero : ℕ → Set
IsNotZero zero    = ⊥
IsNotZero (suc n) = ⊤


data IsNotZero' : ℕ → Set where
  succase : n → IsNotZero' (suc n)

data IsNotZero'(n : ℕ) : Set where
  succase : IsNotZero' (suc n)


-}

-- Vec and Fin
{-
infixr 5 _∷_
data Vec (A : Set) : ℕ → Set where
  []  : Vec A 0
  _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)
-}
head : {A : Set}{n : ℕ} → Vec A (suc n) → A --legalább 1 hosszú vector
head (x ∷ xs) = x

tail : {A : Set}{n : ℕ} → Vec A (suc n) → Vec A n
tail (x ∷ xs) = xs

countDownFrom : (n : ℕ) → Vec ℕ n
countDownFrom zero = []
countDownFrom (suc n) = (suc n) ∷ (countDownFrom n)

test-countDownFrom : countDownFrom 3 ≡ 3 ∷ 2 ∷ 1 ∷ []
test-countDownFrom = refl

_++_ : {A : Set}{m n : ℕ} → Vec A n → Vec A m → Vec A (n + m)
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : {A B : Set}{n : ℕ} → (A → B) → Vec A n → Vec B n
map f [] = []
map f (x ∷ xs) = (f x) ∷ (map f xs)

-- Melyik az a függvény, amit nem tudunk totálisan megírni (még)?
-- Indexelés! Kell hozzá új ötlet!

{-
data Fin : ℕ → Set where  -- Fin n = n-elemu halmaz
  fzero : {n : ℕ} → Fin (suc n)
  fsuc  : {n : ℕ} → Fin n → Fin (suc n)
-}

f0 : Fin 0 → ⊥
f0 ()

f1-0 : Fin 1
f1-0 = fzero

f2-0 f2-1 : Fin 2
f2-0 = fzero
f2-1 = fsuc fzero

f3-0 f3-1 f3-2 : Fin 3
f3-0 = fzero
f3-1 = fsuc fzero
f3-2 = fsuc (fsuc fzero)

f4-0 f4-1 f4-2 f4-3 : Fin 4
f4-0 = fzero
f4-1 = fsuc fzero
f4-2 = fsuc (fsuc fzero)
f4-3 = fsuc (fsuc (fsuc fzero))

--              a < b
data LessThan : ℕ → ℕ → Set where
  zeroCase : {n : ℕ} → LessThan 0 (suc n)
  sucCase  : {n m : ℕ} → LessThan n m → LessThan (suc n) (suc m)

index : {A : Set}{n : ℕ} → Vec A n → (m : ℕ) → LessThan m n → A
index (x ∷ xs) zero zeroCase = x
index (x ∷ xs) (suc m) (sucCase s) = index xs m s

-- Lib-ben a unicode ‼ az indexelés.
infixl 9 _!!_
_!!_ : {A : Set}{n : ℕ} → Vec A n → Fin n → A
[] !! ()
(x ∷ xs) !! fzero = x
(x ∷ xs) !! fsuc n = xs !! n

test-!! : (the ℕ 3 ∷ 4 ∷ 1 ∷ []) !! (fsuc (fsuc fzero)) ≡ 1
test-!! = refl

test2-!! : (the ℕ 3 ∷ 4 ∷ 1 ∷ 0 ∷ 10 ∷ []) !! 3 ≡ 0 -- 3-as literál a !! után valójában Fin 5 típusú.
test2-!! = refl

fromℕ : (n : ℕ) → Fin (suc n)
fromℕ zero = fzero
fromℕ (suc n) = fsuc (fromℕ n)

test-fromℕ : fromℕ 3 ≡ fsuc (fsuc (fsuc fzero))
test-fromℕ = refl

{-
data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A
-}

{-
length : {A : Set} → List A → ℕ
length [] = zero
length (x ∷ xs) = suc (length xs)
-}

fromList : {A : Set}(as : List A) → Vec A (length as)
fromList [] = []
fromList (x ∷ xs) = x ∷ (fromList xs)

tabulate : {n : ℕ}{A : Set} → (Fin n → A) → Vec A n
tabulate {zero} {A} f = []
tabulate {suc n} {A} f = (f fzero) ∷ (tabulate {n} {A} λ x → f (fsuc x))

test-tabulate : tabulate (the (Fin 3 -> ℕ) (λ {fzero -> 6; (fsuc fzero) -> 9; (fsuc (fsuc fzero)) -> 2}))
                  ≡ 6 ∷ 9 ∷ 2 ∷ []
test-tabulate = refl

-- Sigma types

what : Σ ℕ (Vec Bool)
what = 2 , true ∷ false ∷ []

filter : {A : Set}{n : ℕ}(f : A → Bool) → Vec A n → Σ ℕ (Vec A) -- ezen lehet pontosítani, hiszen n elemnél nem kéne legyen benne több elem soha.
filter f [] = 0 , []
filter f (x ∷ xs) = if f x then (suc (fst (filter f xs)) , (x ∷ snd (filter f xs))) else (fst (filter f xs) , snd (filter f xs))

test-filter : filter {ℕ} (3 <ᵇ_) (4 ∷ 3 ∷ 2 ∷ 5 ∷ []) ≡ (2 , 4 ∷ 5 ∷ [])
test-filter = refl

smarterLengthVec : ∀{i}{A : Set i}{n : ℕ} → Vec A n → ℕ
smarterLengthVec {i} {A} {n} x = n

minMax' : ℕ → ℕ → ℕ × ℕ
fst (minMax' n m) = 0
snd (minMax' n m) = 0

-- Ugyanez sokkal jobban, de leginkább pontosabban.
-- Az előző változatban vissza tudok adni csúnya dolgokat is.
-- Pl. konstans (0 , 0)-t.
minMax : (n m : ℕ) → Σ (ℕ × ℕ) (λ (a , b) → a ≤ℕ b × ((n ≤ℕ m × n ≡ a × m ≡ b) ⊎ (m ≤ℕ n × n ≡ b × m ≡ a)))
minMax zero m = (zero , m) , (tt , (inl (tt , (refl , refl)))) 
minMax (suc n) zero = (zero , (suc n)) , (tt , (inr (tt , refl , refl)))
minMax (suc n) (suc m) with (minMax n m)
... | (a , b) , (a≤b , inl (n≤m , n=a , m=b)) = (suc a , suc b) , (a≤b , inl (n≤m , (cong suc n=a , cong suc m=b)))
... | (a , b) , (a≤b , inr (m≤n , n=b , m=a)) = (suc a , suc b) , (a≤b , inr (m≤n , ((cong suc n=b) , (cong suc m=a))))