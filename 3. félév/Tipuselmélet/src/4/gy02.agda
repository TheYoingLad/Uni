module gy02 where

open import Lib hiding (comm×; assoc×; flip; curry; uncurry)

-- Erről kövi óra elején beszélek

-- α-konverzió, renaming
id= : ∀{i}{A : Set i} → (λ (x : A) → x) ≡ (λ y → y)
id= = refl

-- Volt függvények β-szabálya, és η-szabálya.
-- Kövi órán mégtöbbet arról hogy milyen szabályok vannak

------------------------------------------------------
-- simple finite types
------------------------------------------------------

{-

A típusok amiket eddig néztünk:

ℕ    - Természetes számok
Bool - Igaz/Hamis
⊤    - egy elemu típus
⊥    - nulla elemu típus

-}

iteℕ : {C : Set} → C → (C → C) → ℕ → C
iteℕ czero csuc zero = czero
iteℕ czero csuc (suc n) = csuc (iteℕ czero csuc n)

iteBool : {C : Set} → C → C → Bool → C
iteBool ctrue cfalse true = ctrue
iteBool ctrue cfalse false = cfalse

ite⊤ : {C : Set} → C → ⊤ → C
ite⊤ ctt tt = ctt

ite⊥ : {C : Set} → ⊥ → C
ite⊥ ()


-- Feladat: Fordítsuk meg egy rendezett pár két komponensét
flip : ℕ × Bool → Bool × ℕ
flip ( n , b ) = ( b , n )

-- Pattern match Ctrl+c Ctrl+c, majd a változó neve amire akarunk pattern matchelni

flipback : Bool × ℕ → ℕ × Bool
-- \_1 == ₁
flipback (b , snd₁) = snd₁ , b

-- Vegyük észre, hogy az előző két függvényben bármilyen más csúnya dolgot is lehetne csinálni.
-- pl.:
flip_wrong : ℕ × Bool → Bool × ℕ
flip_wrong ( n , b ) = (false , 2)

-- Paraméret felvétel : Ctrl+c Ctrl+c után nem írok semmit

comm× : {A B : Set} → A × B → B × A
comm× (f , s) = s , f

comm×back : {A B : Set} → B × A → A × B
comm×back (f , s) = s , f

-- Ezekben lehetetlen hülyeséget csinálni.
-- Hányféleképpen lehetséges implementálni ezt a két fenti függvényt?


-- ALGEBRAI ADATTÍPUSOK ELEMSZÁMAI:

{-

ℕ    - Végtelen sok, 0,1,2,3,...
Bool - 2
⊤    - 1
⊥    - 0

-}

-- Adjunk meg külömböző Bool × ⊤ típusú dolgokat
b1 b2 : Bool × ⊤
b1 = false , tt
b2 = true  , tt
b1≠b2 : b1 ≡ b2 → ⊥
b1≠b2 ()

-- Összesen ez a kettő van, mert a Bool 2 féle lehet, de a ⊤ csak 1

-- Adjunk meg külömböző (⊤ vagy ⊤) vagyis ⊤ ⊎ ⊤ típusú dolgokat
t1 t2 : ⊤ ⊎ ⊤
t1 = inl tt
t2 = inr tt
t1≠t2 : t1 ≡ t2 → ⊥
t1≠t2 ()

-- Adjunk meg külömböző (Bool vagy ⊤) vagyis Bool ⊎ ⊤ típusú dolgokat
bb1 bb2 bb3 : Bool ⊎ ⊤
bb1 = inl false
bb2 = inl true
bb3 = inr tt
bb1≠bb2 : bb1 ≡ bb2 → ⊥
bb1≠bb2 ()
bb1≠bb3 : bb1 ≡ bb3 → ⊥
bb1≠bb3 ()
bb2≠bb3 : bb2 ≡ bb3 → ⊥
bb2≠bb3 ()

-- A bal oldalit nem tudtuk megadni, mert ott a olyan függvényt kéne írni, ami a 0 elemu típusba tér vissza
ee : (⊤ → ⊥) ⊎ (⊥ → ⊤)
ee = inr (λ x → tt)

-- Hasonlóan itt is.
d : (⊤ ⊎ (⊤ × ⊥)) × (⊤ ⊎ ⊥)
d = inl tt , inl tt

-- Ezt a kettőt amúgy Agda ki tudja találni Ctrl+c Ctrl+a val

-- Ezek alapján hogy lehet megállapítani, hogy melyik típus hány elemű?
-- | ⊤ |        = 1
-- | ⊥ |        = 0
-- | Bool |     = 2, ezek definícióból
-- | Bool ⊎ ⊤ | = 3, fent láttuk
-- | A ⊎ B |    = |A| + |B| (vagy inl-t írok és akkor A-kat tudok írni, ezt |A| féle képpen lehet vagy inr-t, amit |B| féle képpen lehet) 
-- | A × B |    = |A| * |B| (kell egy A ÉS egy B is, vagyis minden kombináció szóba jöhet, hasonló ötlet a decartes szorzat) 
-- | Bool × Bool × Bool | = 2 * 2 * 2 = 8
-- | ⊤ → ⊥ | = 0, fent láttuk
-- | ⊥ → ⊤ | = 1, fent láttuk
-- | ⊥ → ⊥ | = 1, ami (a : ⊥)-ot kapok azt vissza is adom
-- | Bool → ⊥ | = 0 = 0^2
-- | Bool → ⊤ | = 1 = 1^2
-- | ⊤ → Bool | = 2 = 2^1
-- | A → B | = |B| ^ |A|
-- | Bool → Bool → Bool | = 2 ^ (2 ^ 2), mivel jobbra zárójeleződik a _→_


-- Ezek alapján milyen matematikai állítást mond ki és bizonyít a lenti állítás?
-- Válasz:

-- Mondhatja azt hogy : A * A = A ^ 2
from' : {A : Set} → A × A → (Bool → A)
from' (a , a') false = a'
from' (a , a') true = a
to' : {A : Set} → (Bool → A) → A × A
to' f = f true , f false

testfromto1 : {A : Set}{a b : A} → fst (to' (from' (a , b))) ≡ a
testfromto1 = refl
testfromto2 : {A : Set}{a b : A} → snd (to' (from' (a , b))) ≡ b
testfromto2 = refl
testfromto3 : {A : Set}{a b : A} → from' (to' (λ x → if x then a else b)) true ≡ a
testfromto3 = refl
testfromto4 : {A : Set}{a b : A} → from' (to' (λ x → if x then a else b)) false ≡ b
testfromto4 = refl

------------------------------------------------------
-- all algebraic laws systematically
------------------------------------------------------

-- (⊎, ⊥) form a commutative monoid (kommutativ egysegelemes felcsoport)

ite⊎ : {A B C : Set} -> (A -> C) -> (B -> C) -> A ⊎ B -> C
ite⊎ cinl cinr (inl a) = cinl a
ite⊎ cinl cinr (inr b) = cinr b

-- Where segédfüggvényekkel
-- Sokat kell írni
assoc⊎ : {A B C : Set} → (A ⊎ B) ⊎ C ↔ A ⊎ (B ⊎ C)
assoc⊎ = (bal) , jobb
    where
        bal : {A B C : Set} → (A ⊎ B) ⊎ C → A ⊎ (B ⊎ C)
        bal (inl (inl a)) = inl a
        bal (inl (inr b)) = inr (inl b)
        bal (inr c) = inr (inr c)

        jobb : {A B C : Set} → A ⊎ B ⊎ C → (A ⊎ B) ⊎ C
        jobb (inl a) = inl (inl a)
        jobb (inr (inl b)) = inl (inr b)
        jobb (inr (inr c)) = inr c

-- Copattern matching-el
-- A Copattern matching az pattern matching Co-induktív típusokra, ez késöbb lesz
-- Viszont szerencsénkre a _×_ co-induktív
idl⊎ : {A : Set} → ⊥ ⊎ A ↔ A
-- Ilyenkor egyesével megadhatom a fst és snd részét a _×_-nek, olyan mintha a bal és jobb-at írnám meg (kb olyan, ...)
fst idl⊎ (inr a) = a
-- Itt az fst-nél a bemenet egy ⊥ vagy A dolog, viszont mivel ⊥ sose lehet, így Agda okosan kihadja azt az esetet 
snd idl⊎ a = inr a

idr⊎ : {A : Set} → A ⊎ ⊥ ↔ A
fst idr⊎ (inl a) = a
snd idr⊎ a = inl a

comm⊎ : {A B : Set} → A ⊎ B ↔ B ⊎ A
fst comm⊎ (inl a) = inr a
fst comm⊎ (inr b) = inl b
snd comm⊎ (inl a) = inr a
snd comm⊎ (inr b) = inl b

-- (×, ⊤) form a commutative monoid (kommutativ egysegelemes felcsoport)

ite× : {A B C : Set} → (A → B → C) → A × B → C
ite× c_,_ (a , b) = c_,_ a b

assoc× : {A B C : Set} → (A × B) × C ↔ A × (B × C)
fst assoc× ((a , b) , c) = a , b , c
snd assoc× (a , b , c)   = (a , b) , c

idl× : {A : Set} → ⊤ × A ↔ A
fst idl× (tt , a) = a
snd idl× a        = tt , a

idr× : {A : Set} → A × ⊤ ↔ A
fst idr× (a , tt) = a
snd idr× a        = a , tt

-- ⊥ is a null element
-- Itt probáljunk meg pattern matchelni egy ⊥ típusú elemre, mi történik
-- Agda azt mondja hadjuk ki azt az esetet, ezt leneti a : null× ()
null× : {A : Set} → A × ⊥ ↔ ⊥
fst null× ()
snd null× ()

-- distributivity of × and ⊎

dist : {A B C : Set} → A × (B ⊎ C) ↔ (A × B) ⊎ (A × C)
fst dist (a , inl b) = inl (a , b)
fst dist (a , inr c) = inr (a , c)
snd dist (inl (a , b)) = a , inl b
snd dist (inr (a , c)) = a , inr c

-- Innentől házi gyakorlás

-- exponentiation laws

curry : ∀{A B C : Set} → (A × B → C) ↔ (A → B → C)
fst curry f a b = f (a , b)
snd curry f (a , b) = f a b

⊎×→ : {A B C D : Set} → ((A ⊎ B) → C) ↔ (A → C) × (B → C)
fst ⊎×→ f = (λ a → f (inl a)) , (λ b → f (inr b))
snd ⊎×→ (f , g) (inl a) = f a
snd ⊎×→ (f , g) (inr b) = g b

law^0 : {A : Set} → (⊥ → A) ↔ ⊤
fst law^0 f = tt
snd law^0 x ()

law^1 : {A : Set} → (⊤ → A) ↔ A
fst law^1 f = f tt
snd law^1 a tt = a

law1^ : {A : Set} → (A → ⊤) ↔ ⊤
fst law1^ f = tt
snd law1^ tt = λ a → tt

---------------------------------------------------------
-- random isomorphisms
------------------------------------------------------

-- Milyen algebrai állítást mond ki az alábbi típus?
iso1 : {A B : Set} → (Bool → (A ⊎ B)) ↔ ((Bool → A) ⊎ Bool × A × B ⊎ (Bool → B))
fst iso1 f = case (f true) 
    (λ a0 → (case (f false) 
        (λ a1 → inl (λ x → if x then a0 else a1)) 
        (λ b1 → inr (inl (true , (a0 , b1)))))) 
    (λ b0 → case (f false) 
        (λ a1 → inr (inl (false , (a1 , b0)))) 
        (λ b1 → inr (inr λ x → if x then b0 else b1)))
snd iso1 (inl f) a = inl (f a)
snd iso1 (inr (inl (l , a , b))) x = if x then 
    if l then inl a else inr b
    else 
    if l then inr b else inl a
snd iso1 (inr (inr f)) b = inr (f b)

--fst iso1 x = {!  !}  where
--    helpr : {A B : Set} → (Bool → (A ⊎ B)) → Bool → A
--    helpr f l = {! inr (f l)  !}
--    helpl = inl (x false)
--snd iso1 (inl fla) l = inl (fla l)
--snd iso1 (inr (inl (_ , a , b))) _ = inl a
--snd iso1 (inr (inr flb)) l = inr (flb l)

iso2 : {A B : Set} → ((A ⊎ B) → ⊥) ↔ ((A → ⊥) × (B → ⊥))
fst (fst iso2 x) a = x (inl a)
snd (fst iso2 x) b = x (inr b)
snd iso2 (a , b) (inl a₁) = a a₁
snd iso2 (a , b) (inr b₁) = b b₁

iso3 : (⊤ ⊎ ⊤ ⊎ ⊤) ↔ Bool ⊎ ⊤
fst iso3 (inl a) = inr tt
fst iso3 (inr (inl b)) = inl true
fst iso3 (inr (inr c)) = inl false
snd iso3 x = inl tt
testiso3 : fst iso3 (inl tt) ≡ fst iso3 (inr (inl tt)) → ⊥
testiso3 ()
testiso3' : fst iso3 (inl tt) ≡ fst iso3 (inr (inr tt)) → ⊥
testiso3' ()
testiso3'' : fst iso3 (inr (inl tt)) ≡ fst iso3 (inr (inr tt)) → ⊥
testiso3'' ()

iso4 : (⊤ → ⊤ ⊎ ⊥ ⊎ ⊤) ↔ (⊤ ⊎ ⊤)
fst iso4 x = case (x tt) (λ _ → inl tt) (λ _ → inr tt)
snd iso4 (inl a) _ = inl a
snd iso4 (inr b) _ = inr (inr b) 
testiso4 : fst iso4 (λ _ → inl tt) ≡ fst iso4 (λ _ → inr (inr tt)) → ⊥
testiso4 ()
testiso4' : snd iso4 (inl tt) tt ≡ snd iso4 (inr tt) tt → ⊥
testiso4' ()