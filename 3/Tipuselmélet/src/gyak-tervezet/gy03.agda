module gy03 where

open import Lib hiding (_+_; _*_; _-_; _^_; _!; pred; pred'; _>_; _<_; min; max)
open import Lib.Containers.List hiding (length; _++_; map; iteList)

-- η = \eta = \Gh

--------------------------------------------------------
-- β, η szabályok egyszerűen megfogalmazva
--------------------------------------------------------
{-
A β-szabályok lényegében azt határozzák meg, hogy hogy kell számolni az értékeimmel; tehát hogy mit kell csinálni, ha egy konstruktorra alkalmazok egy destruktort.
Pl. függvények esetén (λ n → n + 2) 3, ebben a kifejezésben a λ a konstruktor, a függvényalkalmazás a destruktor; ekkor csak be kell helyettesíteni az értéket a megfelelő helyére,
majd ki kell számolni az értékét.

Az η-szabályok azt határozzák meg, hogy mit kell tenni ha egy destruktorra alkalmazok egy konstruktort.
Pl. függvények esetén (λ x → (1 +_) x), ebben a lambda alatt található egy függvényalkalmazás, amiről az előbb volt szó, hogy az egy destruktor; a λ a konstruktor,
ekkor tudjuk specifikusan függvények esetében, hogy a λ elhagyható az átadott x-szel együtt (λ x → (1 +_) x) ≡ (1 +_)
-}

--------------------------------------------------------
-- típusok β szabályai
--------------------------------------------------------
{-
Minden típusnak megadható a β szabálya a típus alapján.

β-szabályok azt mondják meg, hogy egy típus egy adott értékével mit kell csinálni, hogy megkülönböztessük a típus többi értékétől.

Egyszerűbb ezt az ötletet talán Bool-on szemléltetni:

data Bool : Set where
  false true : Bool

Hogyan lehet megkülönböztetni a false-ot a true-tól?
Kell egy függvény (destruktor), amely különböző Bool értékekre különböző eredményt ad szintaxis szerint, és CSAK és PONTOSAN a Bool értékeit kezeli,
tehát mivel a Bool-nak két értéke van, ezért a destruktornak PONTOSAN két elemet kell kezelnie, nem többet, nem kevesebbet.

Melyik ez a függvény a Bool-ok felett, ami false-ra, illetve true-ra egyértelműen két különböző dolgot ad eredményül? (Akár haskell-ből, akár más oop nyelvekből ismert konstrukció.)
Mi a destruktora?
Válasz:

Hány β-szabályra van szükség a Bool esetén?
Válasz:

Mik lesznek ezek a β-szabályok?
Válasz:
---------------------------------------------------------
Ha írunk egy 3 elemű típust (lényegében csak egy enumot):

data 𝟛 : Set where
  a1 a2 a3 : 𝟛

Mi lesz a 𝟛 típus destruktora?
Válasz:

Akkor ennek a típusnak mik lesznek a β-szabályai?
Válasz:
----
4 elemre:

data 𝟜 : Set where
  b1 b2 b3 b4 : 𝟜

Mi lesz a destruktora?
Válasz:

Mik lesznek ennek a β-szabályai?
Válasz:
----
Mi a ⊤ típus destruktora?
Válasz:

Mi lesz a ⊤ típus β-szabálya?
Válasz:
----
Mi a ⊥ destruktora?
Válasz:

Mi lesz a ⊥ típus β-szabálya?
Válasz:
----------------------------------------------------------
Mi történik abban az esetben, ha vannak a típusoknak paramétereik?

data Alma : Set where
  c1 : Alma
  c2 : Bool → Alma

Természetesen semmi különleges, pontosan ugyanaz fog a destruktorban szerepelni, mint a konstruktorok továbbra is.

Mi lesz a destruktora?
Válasz:

Mik lesznek a β-szabályai?
Válasz:
-----------------------------------------------------------
Mi történik, ha van legalább két paramétere egy konstruktornak?

Pl. rendezett pár: _,_ : A → B → A × B

Semmi, a destruktor továbbra is ugyanúgy generálható (ez természetesen nem azt jelenti, hogy csak az az egy jó van).
Mi lesz a rendezett párok egy destruktora?

Amelyik generálható az eddigiek alapján: uncurry : (A → B → C) → A × B → C

Más destruktorok is jók, pl. ezzel az eggyel ekvivalens az alábbi kettő együtt:
- fst : A × B → A
- snd : A × B → B

Ezek alapján mik a β-szabályok?
Az uncurry-vel csak egy szabály szükséges: uncurry f (a , b) ≡ f a b
Az fst, snd-vel kettő (hiszen két destruktor van egy konstruktorral, 2 ∙ 1 = 2): fst (a , b) ≡ a; snd (a , b) ≡ b
------------------------------------------------------------
data Körte : Set where
  d1 : Körte
  d2 : Bool → Körte
  d3 : Bool → 𝟛 → Körte

Mi lesz ezen típus destruktora?
Válasz:

És a β-szabályai?
Válasz:
-}

---------------------------------------------------------
-- típusok η-szabályai
---------------------------------------------------------
-- Ezt majd a következő gyakorlat elejére rakom be, így is van itt elég tenni való.

---------------------------------------------------------
-- natural numbers, no cheating anymore
---------------------------------------------------------

-- A természetes számok a diszkrét matekról ismert módon vannak megadva,
-- tehát van a 0 és van rákövetkezője.
{-
data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ
-}

-- Haskellből ismert Maybe típus.
{-
data Maybe (A : Set) : Set where
  just : A → Maybe A
  nothing : Maybe A
-}

-- FELADAT: Csökkents eggyel egy megadott természetes számot, ha lehet.
pred' : ℕ → Maybe ℕ
pred' = {!!}

-- FELADAT: Ha lehet, akkor adj hozzá a számhoz egyet, egyébként az eredmény legyen 0.
zerosuc : Maybe ℕ → ℕ
zerosuc = {!!}

pred↔zerosuc-test1 : pred' (zerosuc nothing) ≡ nothing
pred↔zerosuc-test1 = refl
pred↔zerosuc-test2 : {n : ℕ} → pred' (zerosuc (just n)) ≡ just n
pred↔zerosuc-test2 = refl

-- Csúnya pred, mert matematikailag nem azt csinálja, a 0-nak nincs megelőzője, az nem lehet 0.
pred'' : ℕ → ℕ
pred'' zero = zero
pred'' (suc n) = n

-- Ennél sokkal jobb pred-et lehet megadni; Maybe nélkül is lehet.
-- Errefelé halad a tárgy; fontos a pontos specifikáció!
-- Kell egy függvény, ami típust ad vissza.
-- Majd utána rendes pred.

----------------------------------------------------------------------------------------
-- Rekurzió, termination checker
-- Agda CSAK totális függvényeket fogad el.

double : ℕ → ℕ
double = {!!}

double-test1 : double 2 ≡ 4
double-test1 = refl
double-test2 : double 0 ≡ 0
double-test2 = refl
double-test3 : double 10 ≡ 20
double-test3 = refl

half : ℕ → ℕ
half = {!!}

half-test1 : half 10 ≡ 5
half-test1 = refl
half-test2 : half 11 ≡ 5
half-test2 = refl
half-test3 : half 12 ≡ 6
half-test3 = refl

_+_ : ℕ → ℕ → ℕ
_+_ = {!!}
infixl 6 _+_

+-test1 : 3 + 5 ≡ 8
+-test1 = refl
+-test2 : 0 + 5 ≡ 5
+-test2 = refl
+-test3 : 5 + 0 ≡ 5
+-test3 = refl

_*_ : ℕ → ℕ → ℕ
_*_ = {!!}
infixl 7 _*_

*-test1 : 3 * 4 ≡ 12
*-test1 = refl
*-test2 : 3 * 1 ≡ 3
*-test2 = refl
*-test3 : 3 * 0 ≡ 0
*-test3 = refl
*-test4 : 0 * 10 ≡ 0
*-test4 = refl

_^_ : ℕ → ℕ → ℕ
_^_ = {!!}
infixr 8 _^_

^-test1 : 3 ^ 4 ≡ 81
^-test1 = refl
^-test2 : 3 ^ 0 ≡ 1
^-test2 = refl
^-test3 : 0 ^ 3 ≡ 0
^-test3 = refl
^-test4 : 1 ^ 3 ≡ 1
^-test4 = refl
^-test5 : 0 ^ 0 ≡ 1 -- Természetes számok felett ez működik, valós számokon problémás.
^-test5 = refl

_! : ℕ → ℕ
_! = {!!}

!-test1 : 3 ! ≡ 6
!-test1 = refl
!-test2 : 1 ! ≡ 1
!-test2 = refl
!-test3 : 6 ! ≡ 720
!-test3 = refl

_-_ : ℕ → ℕ → ℕ
_-_ = {!!}
infixl 6 _-_

-test1 : 3 - 2 ≡ 1
-test1 = refl
-test2 : 3 - 3 ≡ 0
-test2 = refl
-test3 : 3 - 4 ≡ 0 -- csúnya dolog
-test3 = refl
-- Kivonásból is lehet jobb verziójút írni.

-- FELADAT: Határozd meg, hogy az első szám nagyobb vagy egyenlő-e, mint a második.
_≥_ : ℕ → ℕ → Bool
_≥_ = {!!}

≥test1 : 3 ≥ 2 ≡ true
≥test1 = refl
≥test2 : 3 ≥ 3 ≡ true
≥test2 = refl
≥test3 : 3 ≥ 4 ≡ false
≥test3 = refl

-- ne hasznalj rekurziot, hanem hasznald _≥_-t!
-- FELADAT: Remélhetőleg értelemszerű.
_>_ : ℕ → ℕ → Bool
_>_ = {!!}

>test1 : 3 > 2 ≡ true
>test1 = refl
>test2 : 3 > 3 ≡ false
>test2 = refl
>test3 : 3 > 4 ≡ false
>test3 = refl

-- ne hasznalj rekurziot
-- FELADAT: Remélhetőleg értelemszerű.
_<_ : ℕ → ℕ → Bool
_<_ = {!!}

<test1 : 3 < 2 ≡ false
<test1 = refl
<test2 : 3 < 3 ≡ false
<test2 = refl
<test3 : 3 < 4 ≡ true
<test3 = refl

-- FELADAT: Két szám közül add vissza a kisebbet.
min : ℕ → ℕ → ℕ
min = {!!}

min-test1 : min 3 2 ≡ 2
min-test1 = refl
min-test2 : min 2 3 ≡ 2
min-test2 = refl
min-test3 : min 3 3 ≡ 3
min-test3 = refl

-- FELADAT: Hasonlíts össze két számot! Ha az első kisebb, mint a második, akkor a harmadik paramétert add vissza; ha egyenlők, akkor a negyediket; ha nagyobb, akkor az ötödiket.
comp : {A : Set} → ℕ → ℕ → A → A → A → A
comp m n m<n m=n m>n = {!!}

comp-test1 : comp {ℕ} 10 10 0 1 2 ≡ 1
comp-test1 = refl
comp-test2 : comp {ℕ} 10 11 0 1 2 ≡ 0
comp-test2 = refl
comp-test3 : comp {ℕ} 12 11 0 1 2 ≡ 2
comp-test3 = refl

-- FELADAT: Határozd meg két szám legnagyobb közös osztóját.
-- Segítség: Használd a comp-ot!
gcd : ℕ → ℕ → ℕ
-- {-# TERMINATING #-} -- Csalás! De ezt a függvényt nem egyszerű jól definiálni ahhoz, hogy agda lássa, hogy terminál.
gcd m n = {!!}

gcd-test1 : gcd 6 9 ≡ 3
gcd-test1 = refl
gcd-test2 : gcd 100 150 ≡ 50
gcd-test2 = refl
gcd-test3 : gcd 17 19 ≡ 1
gcd-test3 = refl
gcd-test4 : gcd 12 24 ≡ 12
gcd-test4 = refl
gcd-test5 : gcd 19 17 ≡ 1
gcd-test5 = refl

-- hasznald ugyanazt a definiciot, mint gcd-nel, de most fuel szerinti rekurzio
gcd-helper : ℕ → ℕ → ℕ → ℕ
gcd-helper zero m n = 42
gcd-helper (suc fuel) m n = {!!}
gcd' : ℕ → ℕ → ℕ
gcd' m n = gcd-helper (m + n) m n

-- Ezt miért fogadja el agda?

gcd'-test1 : gcd' 6 9 ≡ 3
gcd'-test1 = refl
gcd'-test2 : gcd' 100 150 ≡ 50
gcd'-test2 = refl
gcd'-test3 : gcd' 17 19 ≡ 1
gcd'-test3 = refl
gcd'-test4 : gcd' 12 24 ≡ 12
gcd'-test4 = refl
gcd'-test5 : gcd' 19 17 ≡ 1
gcd'-test5 = refl

-- FELADAT: Páros-e egy szám?
even? : ℕ → Bool
even? = {!!}

even?-test1 : even? 3 ≡ false
even?-test1 = refl
even?-test2 : even? 200 ≡ true
even?-test2 = refl

-- FELADAT: Határozd meg a Fibonacci-sorozat n. elemét; a 0. eleme legyen 1.
fib : ℕ → ℕ
fib = {!!}

fib-test1 : fib 6 ≡ 13
fib-test1 = refl
fib-test2 : fib 3 ≡ 3
fib-test2 = refl

-- FELADAT: Vizsgáld meg, hogy két szám egyenlő-e! Ne használj rekurziót!
eq? : ℕ → ℕ → Bool
eq? = {!!}

eq?-test1 : eq? 4 3 ≡ false
eq?-test1 = refl
eq?-test2 : eq? 4 4 ≡ true
eq?-test2 = refl

-- rem m n = a maradek, ha elosztjuk m-et (suc n)-el
-- FELADAT: Két számot osszunk el, az eredmény legyen az egész osztás maradéka.
rem : ℕ → ℕ → ℕ
rem a b = {!!}
rem-test1 : rem 5 1 ≡ 1
rem-test1 = refl
rem-test2 : rem 11 2 ≡ 2
rem-test2 = refl

-- div m n = m-ben hanyszor van meg (suc n)
-- FELADAT: Két számot egész osszunk!
div : ℕ → ℕ → ℕ
div a b = {!!}
div-test1 : div 5 1 ≡ 2
div-test1 = refl
div-test2 : div 11 2 ≡ 3
div-test2 = refl

-- Miért ite-vel kezdődik a neve?
iteNat : {A : Set} → A → (A → A) → ℕ → A
iteNat z s zero = z
iteNat z s (suc n) = s (iteNat z s n)

recNat : {A : Set} → A → (ℕ → A → A) → ℕ → A
recNat z s zero = z
recNat z s (suc n) = s n (recNat z s n)

-- FEL: add meg iteNat-ot mintaillesztes nelkul, recNat segitsegevel
iteNat' : {A : Set} → A → (A → A) → ℕ → A
iteNat' = {!!}

iteNat'-test1 : {A : Set}{z : A}{s : A → A} → iteNat' z s zero ≡ z
iteNat'-test1 = refl
iteNat'-test2 : {A : Set}{z : A}{s : A → A}{n : ℕ} → iteNat' z s (suc n) ≡ s (iteNat' z s n)
iteNat'-test2 = refl

-- FEL: add meg recNat-ot mintaillesztes nelkul, iteNat segitsegevel (lasd eloadas)
recNat' : {A : Set} → A → (ℕ → A → A) → ℕ → A
recNat' = {!!}

recNat'-test1 : {A : Set}{z : A}{s : ℕ → A → A} → recNat' z s zero ≡ z
recNat'-test1 = refl
recNat'-test2 : {A : Set}{z : A}{s : ℕ → A → A} → recNat' z s 3 ≡ s 2 (s 1 (s 0 z))
recNat'-test2 = refl

-- FEL: add meg ujra az osszes fent fuggvenyt mintaillesztes nelkul, iteNat es/vagy recNat hasznalataval!

---------------------------------------------------------
-- lists
---------------------------------------------------------

{-
data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A
infixr 5 _∷_
-}

-- FELADAT: Határozzuk meg egy lista elemszámát!
length : {A : Set} → List A → ℕ
length = {!!}

length-test1 : length {ℕ} (1 ∷ 2 ∷ 3 ∷ []) ≡ 3
length-test1 = refl
length-test2 : length {ℕ} (1 ∷ []) ≡ 1
length-test2 = refl

-- FELADAT: Adjuk össze egy lista számait.
sumList : List ℕ → ℕ
sumList = {!!}

sumList-test : sumList (1 ∷ 2 ∷ 3 ∷ []) ≡ 6
sumList-test = refl

-- FELADAT: Fűzzünk össze két listát!
_++_ : {A : Set} → List A → List A → List A
_++_ = {!!}
infixr 5 _++_

++-test : the ℕ 3 ∷ 2 ∷ [] ++ 1 ∷ 4 ∷ [] ≡ 3 ∷ 2 ∷ 1 ∷ 4 ∷ []
++-test = refl

-- FELADAT: Alkalmazzunk egy függvényt egy lista minden elemén!
map : {A B : Set} → (A → B) → List A → List B
map = {!!}

map-test : map (_+ 2) (3 ∷ 9 ∷ []) ≡ (5 ∷ 11 ∷ [])
map-test = refl

-- FELADAT: Definiáld a lista destruktorát! Dolgozzunk fel egy listát:
-- ha üres a lista, akkor csak adjunk vissza egy alapértéket
-- ha a listában van elem, akkor alkalmazzunk rá egy függvényt az alapértékkel úgy, hogy az kifejezés jobbra legyen zárójelezve.
-- Haskell-ben foldr
iteList : {A B : Set} → B → (A → B → B) → List A → B
iteList n c as = {!!}

iteList-test : iteList {ℕ} [] _∷_ (1 ∷ 2 ∷ 3 ∷ []) ≡ 1 ∷ 2 ∷ 3 ∷ []
iteList-test = refl

-- FEL: add meg a fenti fuggvenyeket (length, ..., map) iteList segitsegevel!

---------------------------------------------------------
-- trees
---------------------------------------------------------

-- a datatype of expressions

data Expr : Set where
  value : ℕ → Expr
  _[+]_ : Expr → Expr → Expr
  _[*]_ : Expr → Expr → Expr

-- 2 * (3 + 4) reprezentacioja:
e : Expr
e = value 2 [*] (value 3 [+] value 4)
{-
  *
 / \
2   +
   / \
  3   4
-}

-- FELADAT: Értékeljünk ki egy kifejezést!
eval : Expr → ℕ
eval = {!!}

eval-test : eval e ≡ 14
eval-test = refl

-- FELADAT: Határozzuk meg egy kifejezésfa (szintaxisfa, AST) magasságát. Levél magassága 0.
height : Expr → ℕ
height = {!!}

height-test : height e ≡ 2
height-test = refl


-- http://www.cs.nott.ac.uk/~psztxa/mgs.2021/datatypes.pdf -ben a 3. feladat (74. oldal):

data Tree (A : Set) : Set where
  leaf : Tree A
  node : Tree A → A → Tree A → Tree A

t : Tree ℕ
t = node (node leaf 1 (node leaf 2 leaf)) 5 leaf
{-
    5
   / \
  1
 / \
    2
   / \
-}

-- FELADAT: Csináljuk meg egy fa inorder bejárását!
tree2List : {A : Set} → Tree A → List A
tree2List = {!!}

tree2List-test : tree2List t ≡ 1 ∷ 2 ∷ 5 ∷ []
tree2List-test = refl

-- egy fa rendezett, ha minden csomopontnal levo erteknel a bal reszfaban kisebb, a kobb reszfaban pedig nagyobb ertekek vannak. peldaul t rendezett
-- segítség: használjuk a _≤_ függvényt.
-- ez a fuggveny egy rendezett faba illeszt be egy uj erteket ugy,
-- hogy a fa rendezett maradjon
insert : ℕ → Tree ℕ → Tree ℕ
insert = {!!}

t' : Tree ℕ
t' = node (node (node leaf 0 leaf) 1 (node leaf 2 leaf)) 5 leaf
{-
      5
     / \
    1
   / \
  0   2
 / \ / \
-}

insert-test : insert 0 t ≡ t'
insert-test = refl

-- FELADAT: egy listát egy rendezett fara alakít.
list2tree : List ℕ → Tree ℕ
list2tree = {!!}

-- FELADAT: Rendezzünk egy listát úgy, hogy azt fává alakítjuk megfelelően, majd inorder bejárjuk!
tree-sort : List ℕ → List ℕ
tree-sort = {!!}

tree-sort-test1 : tree-sort (10 ∷ 2 ∷ 1 ∷ 5 ∷ []) ≡ 1 ∷ 2 ∷ 5 ∷ 10 ∷ []
tree-sort-test1 = refl

tree-sort-test2 : tree-sort (1 ∷ 2 ∷ 1 ∷ 5 ∷ []) ≡ 1 ∷ 1 ∷ 2 ∷ 5 ∷ []
tree-sort-test2 = refl

-- nested types

data RoseTree : Set where
  node : List RoseTree → RoseTree

tR : RoseTree
tR = node (node (node [] ∷ []) ∷ node [] ∷ node (node [] ∷ node [] ∷ []) ∷ [])
{-
  /|\
 |  /\
-}

-- FELADAT: Számoljuk meg egy rózsafa csomópontjait.
countNodes     : RoseTree → ℕ
countNodesList : List RoseTree → ℕ
countNodes = {!!}
countNodesList = {!!}

countNodes-test : countNodes tR ≡ 7
countNodes-test = refl

-- maximum of two numbers
max : ℕ → ℕ → ℕ
max = {!!}

max-test1 : max 3 2 ≡ 3
max-test1 = refl
max-test2 : max 20 30 ≡ 30
max-test2 = refl
max-test3 : max 20 20 ≡ 20
max-test3 = refl

-- FELADAT: Határozzuk meg egy rózsafa magasságát.
heightRoseTree : RoseTree → ℕ
heightRoseTreeList : List RoseTree → ℕ
heightRoseTree = {!!}
heightRoseTreeList = {!!}

heightRoseTree-test1 : heightRoseTree tR ≡ 2
heightRoseTree-test1 = refl
heightRoseTree-test2 : heightRoseTree (node (node (node (node [] ∷ []) ∷ []) ∷ [])) ≡ 3
heightRoseTree-test2 = refl

-- vegtelenul elagazodo fak (infinitely branching trees)

data TreeInf : Set where
  leaf : TreeInf
  node : (ℕ → TreeInf) → TreeInf

-- a balanced tree which has height two (draw it!)
t2 : TreeInf
t2 = node (λ _ → node (λ _ → leaf))

-- tI n should be a complete tree of height n (all branches should have height n-1, and so on)
tI : ℕ → TreeInf
tI = {!!}

tI-test1 : tI 3 ≡ node λ _ → node λ _ → node λ _ → leaf
tI-test1 = refl
tI-test2 : tI 5 ≡ node λ _ → node λ _ → node λ _ → node λ _ → node λ _ → leaf
tI-test2 = refl

-- a tree where the height of the n^th branch is n (all branches have finite length, but there is no upper bound)
tI' : TreeInf
tI' = {!!}

_!_ : TreeInf → ℕ → TreeInf
leaf ! n = leaf
node ts ! n = ts n
test-tI'1 : tI' ! 0 ≡ leaf
test-tI'1 = refl
test-tI'2 : tI' ! 1 ≡ node λ _ → leaf
test-tI'2 = refl
test-tI'3 : tI' ! 3 ≡ node λ _ → node λ _ → node λ _ → leaf
test-tI'3 = refl
test-tI'4 : tI' ! 5 ≡ node λ _ → node λ _ → node λ _ → node λ _ → node λ _ → leaf
test-tI'4 = refl
