module gy04 where

open import Lib hiding (_+∞_; coite-ℕ∞; ⊤η)

open import Lib.Containers.List hiding (zipWith; head; tail)
open import Lib.Containers.Stream hiding (zipWith; coiteStream)

---------------------------------------------------------
-- típusok η-szabályai
---------------------------------------------------------
{-
Emlékeztető: A η-szabály azt mondja meg, hogy mit tegyünk, ha destruktorra alkalmazunk konstruktort.
Pl. függvények esetén (λ a → f a) ≡ f, ahol a függvényalkalmazás a destruktor és a λ a konstruktor.

Természetesen más típusoknak is ugyanúgy van η-szabálya.

Vegyük példaként a ⊤-ot:
Destruktora: ite⊤ : A → ⊤ → A

Ez alapján az η-szabály az alábbi lesz:
ite⊤ tt x ≡ x

Ez természetesen Agdában bizonyítható is.
-}

ite⊤ : ∀{i}{A : Set i} → A → ⊤ → A
ite⊤ x _ = x

⊤η : ∀{x} → ite⊤ tt x ≡ x
⊤η = refl

{-
Ahogy emlékeztek rá, a ⊤ η-szabálya úgy néz ki, hogy ∀ a → a ≡ tt,
tehát itt is igaz lesz, hogy egy típusnak több egymással ekvivalens η-szabálya lehet.

Nézzük újra példaként a Bool típust. A β-szabályai a következők voltak:
if true then u else v ≡ u
if false then u else v ≡ v

Mi lehet az η-szabály? Hogy lehet "destruktorra alkalmazni konstruktort" ilyen esetben?
Az if_then_else_ esetén a "then" és az "else" ágban lévő dolgok tetszőleges értékek lehetnek;
ide akár konstruktort is be lehet írni. Tehát úgy lehet felépíteni az η-szabályokat, hogy a destruktor megfelelő
helyeire beírom az azonos típus konstruktorait.
Bool esetén ez azt jelenti, hogy az if_then_else_-ben a második és harmadik helyre kell a Bool két konstruktorát írni.
Ezen felül úgy kell beírni a két konstruktort, hogy alapvetően az "identitás" függvényt kapjuk az adott típuson.
Bool esetén tehát úgy kell az if_then_else_-et felparaméterezni, hogy a false-ra false legyen az eredmény, true-ra pedig true.

Ez alapján mi lesz a Bool-oknak egy lehetséges η-szabálya?
Válasz:

Ugyanezt az ismert 𝟛 típuson is el lehet játszani.
data 𝟛 : Set where
  a1 a2 a3 : 𝟛

Ismert a destruktor: ite𝟛 : A → A → A → 𝟛 → A

Mi lesz a 𝟛 η-szabálya?
Válasz:

Természetes számokon a helyzet szintén nem változik.
Ismert a destruktor: iteℕ : A → (A → A) → ℕ → A

Mi lesz ℕ η-szabálya?
Válasz:

-}

---------------------------------------------------------
-- positivity
---------------------------------------------------------

-- Miért nem enged agda bizonyos típusokat definiálni? Pl. alapesetben az alábbit sem.

{-# NO_POSITIVITY_CHECK #-}
data Tm : Set where
  lam : (Tm → Tm) → Tm

-- FELADAT: Tm-ből adjuk vissza a lam értékét.
app : Tm → (Tm → Tm)
app = {!!}

self-apply : Tm
self-apply = lam (λ t → app t t)

-- C-c C-n this:
Ω : Tm
Ω = app self-apply self-apply

{-# NO_POSITIVITY_CHECK #-}
data Weird : Set where
  foo : (Weird → ⊥) → Weird
  -- Hogy kell elolvasni magyarul a "foo" konstruktort?

unweird : Weird → ⊥
unweird = {!!}

-- ⊥ típusú értéknek TILOS léteznie, ellenkező esetben a rendszer inkonzisztens, nem használható SEMMIRE.
bad : ⊥
bad = {!!}

---------------------------------------------------------
-- coinductive types
---------------------------------------------------------

{-
record Stream (A : Set) : Set where
  coinductive
  field
    head : A
    tail : Stream A
open Stream
-}
-- check that the type of head : Stream A → A
--                        tail : Stream A → Stream A

-- Ez a típus lényegében a végtelen listákat kódolja el.
-- Ebben véges lista nincs benne, csak végtelen!


-- Copattern matching!
-- FELADAT: Add meg azt a végtelen listát, amely csak 0-kból áll.
zeroes : Stream ℕ
zeroes = {!!}
-- Honnan tudja agda, hogy ez totális?
-- Termination checker nem tud futni, hiszen a lista végtelen.
-- Productivity checker

-- by pattern match on n
-- FELADAT: Add meg azt a listát, amely n-től 0-ig számol vissza egyesével.
countDownFrom : ℕ → List ℕ
countDownFrom n = {!!}

-- from n is not by pattern match on n
-- copattern match on Stream
-- FELADAT: Adjuk meg azt a végtelen listát, amely n-től 1-esével felfelé számol!
from : ℕ → Stream ℕ
from n = {!!}

-- pointwise addition
zipWith : {A B C : Set} → (A → B → C) → Stream A → Stream B → Stream C
zipWith = {!!}

-- Definiálható-e a filter sima listákon?
filterL : {A : Set} → (A → Bool) → List A → List A
filterL = {!!}

-- Definiálható-e a filter Stream-eken?
filterS : {A : Set} → (A → Bool) → Stream A → Stream A
filterS P xs = {!!}

-- one element from the first stream, then from the second stream, then from the first, and so on
interleave : {A : Set} → Stream A → Stream A → Stream A
interleave = {!!}

-- get the n^th element of the stream
get : {A : Set} → ℕ → Stream A → A
get = {!!}

-- byIndices [0,2,3,2,...] [1,2,3,4,5,...] = [1,3,4,2,...]
byIndices : {A : Set} → Stream ℕ → Stream A → Stream A
byIndices = {!!}

-- iteℕ : (A : Set) → A → (A → A)  → ℕ → A
--        \______________________/
--         ℕ - algebra

-- Mi lesz a Stream konstruktora?
coiteStream : {A B : Set} → ?
--               \_______________________________/
--                        Stream A - coalgebra
coiteStream = ?

-- ex: redefine the above functions using coiteStream

-- ex: look at conatural numbers in Thorsten's book and do the exercises about them

-- simple calculator (internally a number, you can ask for the number, add to that number, multiply that number, make it zero (reset))
record Machine : Set where
  coinductive
  field
    getNumber : ℕ
    add       : ℕ → Machine
    mul       : ℕ → Machine
    reset     : Machine
open Machine

calculatorFrom : ℕ → Machine
calculatorFrom n = {!!}

c0 c1 c2 c3 c4 c5 : Machine
c0 = calculatorFrom 0
c1 = add c0 3
c2 = add c1 5
c3 = mul c2 2
c4 = reset c3
c5 = add c4 2

-- FELADAT: Készítsünk egy csokiautomatát.
-- A gépbe tudunk pénzt dobálni (ez legyen ℕ, ennyit adjunk hozzá a jelenlegi kreditünhöz).
-- A tranzakciót meg tudjuk szakítani, a kredit 0 lesz és visszaadja a pénzünket.
-- Legyen 3 termékünk, ezek egyenként kerülnek valamennyibe és van belőlük a gépben valamennyi.
-- + Twix: 350-be kerül, kezdetben van a gépben 50 darab.
-- + Croissant: 400-ba kerül, kezdetben van 75 darab.
-- + Snickers: 375-be kerül, kezdetben van 60 darab.
-- Tudunk 1 terméket vásárolni, ha van elég bedobott pénzünk, ekkor a darabszámból vonjunk le egyet (ha lehet) és adjuk vissza a visszajárót, a kreditet nullázzuk le.
-- A gép tartalmát újra tudjuk tölteni, ekkor twix-ből legyen újra 50 darab, croissant-ból 75, snickers-ből pedig 60.

-- conatural numbers
{-
record ℕ∞ : Set where
  coinductive
  field
    pred∞ : Maybe ℕ∞
open ℕ∞
-}

_+∞_ : ℕ∞ → ℕ∞ → ℕ∞
_+∞_ = {!!}

-- Ez a függvény létezik, ezzel lehet megnézni
-- egy conat tényleges értékét.
-- Az első paraméter a fuel, maximum ezt a természetes számot tudja visszaadni.
-- Második paraméter a conat, amire kíváncsiak vagyunk.
-- Értelemszerűen ∞-re mindig nothing az eredmény.
{-
ℕ∞→ℕ : ℕ → ℕ∞ → Maybe ℕ
ℕ∞→ℕ zero _ = nothing
ℕ∞→ℕ (suc n) c with pred∞ c
... | zero∞ = just 0
... | suc∞ b with ℕ∞→ℕ n b
... | nothing = nothing
... | just x = just (suc x)
-}

coiteℕ∞ : {B : Set} → (B → Maybe B) → B → ℕ∞
coiteℕ∞ = {!!}

-- TODO, further exercises: network protocols, simple machines: chocolate machine (input: coin, getChocolate, getBackCoins, output: error, chocolate, money back), some Turing machines, animations, IO, repl, shell
