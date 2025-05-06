module gy06 where

open import Lib hiding (K)

--------------------------------------------------------
-- Elmélet: Függőtípusok elemszámai
--------------------------------------------------------
{-
A függőtípusok is algebrai adattípusoknak számítanak, így természetesen a függőtípusok elemszámai is megadhatók könnyedén.
Nem véletlen, hogy a típusok nevei rendre Σ és Π.

Emlékeztetőül:
| A ⊎ B | = |A| + |B|
| A × B | = |A| ∙ |B|
| A → B | = |B| ^ |A|

Tfh:
P : Bool → Set
P true = Bool
P false = ⊥

Σ Bool P hány elemű lesz?
Ha a Bool-om true (1 konkrét érték), akkor Bool típusú eredményt kell a másik részbe írnom, tehát eddig 2¹ = 2
De ha a Bool-om false (1 konkrét érték), akkor ⊥ típusú eredmény kell, tehát 0¹ = 0

Tehát a nap végén | Σ Bool P | = |Bool| + |⊥| = 2, mert a P egy Bool-tól függő típust ad eredményül, tehát maga a típus vagy P true típusú értéket tartalmaz vagy P false-ot.
Az ilyen "vagy" kapcsolatról megbeszéltük korábban, hogy az az összeadást jelenti.

Legyen most:
P : Maybe Bool → Set
P nothing = ⊤
P (just true) = Bool
P (just false) = Maybe Bool → Bool

Hány elemű lesz Σ (Maybe Bool) P? Igazából a típus egyes elemei alapján csak meg kell nézni, hogy hány elemű típust adnak vissza.
| Σ (Maybe Bool) P | = | P nothing | + | P (just true) | + | P (just false) | = |⊤| + |Bool| + |Maybe Bool → Bool| = 1 + 2 + 8 = 11

Ez alapján az intuíció az lehet, hogy | Σ A B | = Σ (i : A) |B i|; tehát csak össze kell adni az egyes típusokból képzett új típusok elemszámát és ennyi.
(Nem véletlen, hogy Σ a típus neve. Ellenőrizhető, hogy A × B elemszáma könnyen ki fog jönni, hogy ha B nem függőtípus.)

A × B = Σ A (λ _ → B)
| A | * | B | = Σ (a : A) | B | -- (A elemszámaszor a B elemszáma)

Mi a helyzet, ha ugyanezt játszuk Π-vel? Hány elemű lesz Π A B?

Megint konkrét helyzetben, legyen:
P : Bool → Set
P true = Bool
P false = ⊥

| Π Bool P | =⟨ Agda szintaxissal ⟩= | (b : Bool) → P b | kell.
A függvényeknek totálisaknak kell lenniük, tehát ez azt jelenti, hogy MINDEN lehetséges b : Bool értékre P b-nek definiáltnak kell lennie, false-ra ÉS true-ra is.
Intuíció alapján | P true | ÉS | P false | kelleni fog, az ÉS kapcsolat matematikában a szorzást szokta jelenteni, tehát | P true | ∙ | P false | elemszámú lesz
ez a kifejezés.
| P true | ∙ | P false | = |Bool| ∙ |⊥| = 2 ∙ 0 = 0
Próbáljuk meg definiálni ezt a függvényt:
-}
P₁ : Bool → Set
P₁ true = Bool
P₁ false = ⊥

ΠBoolP : Π Bool P₁
(ΠBoolP false) = {!   !}
ΠBoolP true = true
-- Rájöhetünk, hogy a false ággal gondok lesznek.
{-
Következő példa, ez a P már ismerős:
P : Maybe Bool → Set
P nothing = ⊤
P (just true) = Bool
P (just false) = Maybe Bool → Bool

Hány elemű lesz Π (Maybe Bool) P?
A függvények továbbra is totálisak kell legyenek. Ez azt jelenti, hogy a függvény definiálva kell legyen (P nothing)-ra, (P (just true))-ra ÉS (P (just false))-ra is,
tehát lesz | P nothing | ∙ | P (just true) | ∙ | P (just false) | elemünk, |⊤| ∙ |Bool| ∙ |Maybe Bool → Bool| = 1 ∙ 2 ∙ 2³ = 16 elemű lesz Π (Maybe Bool) P.

Intuíció alapján általánosan | Π A B | = Π (i : A) | B i |, tehát csak az összes B-ből képezhető típus elemszámát össze kell szorozni.

Gyakorlás:
Adott a következő P:

P : Bool × Bool → Set 
P (true , true) = ⊤
P (true , false) = Bool
P (false , true) = Bool → Bool ⊎ ⊤
P (false , false) = Bool ⊎ ⊤ → Bool

Hány elemű lesz Σ (Bool × Bool) P?
|⊤| + |Bool| + |Bool → Bool ⊎ ⊤| + |Bool ⊎ ⊤ → Bool| = 1 + 2 + 3² + 2³ = 20

Hány elemű lesz Π (Bool × Bool) P?
|⊤| * |Bool| * |Bool → Bool ⊎ ⊤| * |Bool ⊎ ⊤ → Bool| = 1 * 2 * 3² * 2³ = 144

Kicsit érdekesebb ezeket vegyíteni, de az elv ugyanaz marad.
Marad ugyanaz a P.

Hány elemű lesz Π (a : Bool) (Σ (b : Bool) (P (a , b)))?
|Σ Bool (λ b → P true b)|        * |Σ Bool (λ b → P false b)|
(|P true true| + |P true false|) * (|P false true|    +  |P false false|)
(|⊤|           + |Bool|)         * (|Bool → Bool ⊎ ⊤| + |Bool ⊎ ⊤ → Bool|) = (1 + 2) * (3² + 2³) = 3 * 17 = 51

Hány elemű lesz Σ (a : Bool) (Π (b : Bool) (P (a , b)))?
|Π Bool (λ b → P true b)|         + |Π Bool (λ b → P false b)|
(|P true true| * |P true false|)  + (|P false true|     * |P false false|)
(|⊤|           * |Bool|)          + (|Bool → Bool ⊎ ⊤| * |Bool ⊎ ⊤ → Bool|) = (1 * 2) + (3² * 2³) = 2 + 72 = 74
-}

----------------------------------------------
-- Some Sigma types
----------------------------------------------

Σ=⊎ : {A B : Set} → Σ Bool (λ b → if b then A else B) ↔ A ⊎ B
fst Σ=⊎ (true , a) = inl a
fst Σ=⊎ (false , b) = inr b
snd Σ=⊎ (inl a) = true , a
snd Σ=⊎ (inr b) = false , b

Σ=× : {A B : Set} → Σ A (λ _ → B) ↔ A × B
fst Σ=× (a , b) = a , b
snd Σ=× (a , b) = a , b

-- Π A F is essentially (a : A) → F a
-- what does this mean?

--                  -- Π A (λ _ → B)
Π=→ : {A B : Set} → ((a : A) → (λ _ → B) a) ≡ (A → B)
Π=→ = refl

--                  -- Π Bool (λ b → if b then A else B)
→=× : {A B : Set} → ((b : Bool) → if b then A else B) ↔ A × B
fst →=× f = (f true) , (f false)
snd →=× x true = fst x
snd →=× x false = snd x

dependentCurry : {A : Set}{B : A → Set}{C : (a : A) → B a → Set} →
  ((a : A)(b : B a) → C a b) ↔ ((w : Σ A B) → C (fst w) (snd w))
fst dependentCurry f (a , Ba) = f a Ba
snd dependentCurry f a Ba = f (a , Ba)


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
module Formalise(SutANap EsikAzEso VanSzivarvany KellAzEsernyo : Set) where

  -- Nem süt a nap.
  form1 : Set
  form1 = ¬ SutANap

  -- Esik az eső és süt a nap.
  form2 : Set
  form2 = EsikAzEso × SutANap

  -- Nem kell az esernyő vagy esik az eső.
  form3 : Set
  form3 = (¬ KellAzEsernyo) ⊎ EsikAzEso

  -- Ha esik az eső és süt a nap, akkor van szivárvány.
  form4 : Set
  form4 = (EsikAzEso × SutANap) → VanSzivarvany

  -- Van szivárvány.
  K : Set
  K = VanSzivarvany

---- Következményfogalom (logika tárgy 1-3. gyakorlat)
  -- Agdában legegyszerűbben szintaktikus következményekkel lehet foglalkozni.

  -- Mondd ki, és bizonyítsd be, hogy a fenti állításokból következik a K.
  -- A típusban kell kimondani az állítást; az állítás kimondásához az eldöntésprobléma tételét kell használni.
  -- Két féleképpen lehet bizonyítani.

  Köv : Set
  Köv = ¬ (form1 × form2 × form3 × form4 × ¬ K)

  Köv1 : Köv
  Köv1 (f1 , f2 , f3 , f4 , nK) = nK (f4 f2)

  Köv2 : Köv
  Köv2 (f1 , f2 , f3 , f4 , nK) = f1 (snd f2)

----------------------------------------------------------------------------

subt-prod : {A A' B B' : Set} → (A → A') → (B → B') → A × B → A' × B'
subt-prod fa fb (a , b) = (fa a) , (fb b)

subt-fun : {A A' B B' : Set} → (A → A') → (B → B') → (A' → B) → (A → B')
subt-fun fa fb fa'b a = fb (fa'b (fa a))

anything : {X Y : Set} → ¬ X → X → Y
anything nx x = exfalso (nx x)

ret : {X : Set} → X → ¬ ¬ X
ret x f = f x

fun : {X Y : Set} → (¬ X) ⊎ Y → (X → Y)
fun (inl nx) x = exfalso (nx x)
fun (inr y) x = y

-- De Morgan

dm1 : {X Y : Set} →  ¬ (X ⊎ Y) ↔ ¬ X × ¬ Y
fst (fst dm1 nxory) x = nxory (inl x)
snd (fst dm1 nxory) y = nxory (inr y)
snd dm1 (nx , ny) (inl x) = exfalso (nx x)
snd dm1 (nx , ny) (inr y) = exfalso (ny y)

dm2 : {X Y : Set} → ¬ X ⊎ ¬ Y → ¬ (X × Y)
dm2 (inl nx) (x , y) = nx x
dm2 (inr ny) (x , y) = ny y

dm2b : {X Y : Set} → ¬ ¬ (¬ (X × Y) → ¬ X ⊎ ¬ Y)
dm2b f = f (λ nxandy → inl λ x → f (λ _ → inr λ y → nxandy (x , y)))

-- stuff

nocontra : {X : Set} → ¬ (X ↔ ¬ X)
nocontra (f₁ , f₂) = f₁ (f₂ λ x → f₁ x x) (f₂ λ x → f₁ x x)

¬¬invol₁ : {X : Set} → ¬ ¬ ¬ ¬ X ↔ ¬ ¬ X
fst ¬¬invol₁ nnnnx nx = nnnnx λ nnx → nnx nx
snd ¬¬invol₁ nnx nnnx = nnnx nnx

¬¬invol₂ : {X : Set} → ¬ ¬ ¬ X ↔ ¬ X
fst ¬¬invol₂ nnnx x = nnnx (λ nx → nx x)
snd ¬¬invol₂ nx nnx = nnx nx

nnlem : {X : Set} → ¬ ¬ (X ⊎ ¬ X)
nnlem nxornx = nxornx (inr (λ x → nxornx (inl x)))

nndnp : {X : Set} → ¬ ¬ (¬ ¬ X → X)
nndnp f = f (λ g → exfalso (g (λ x → f (λ _ → x))))

dec2stab : {X : Set} → (X ⊎ ¬ X) → (¬ ¬ X → X)
dec2stab (inl x) nnx = x
dec2stab (inr nx) nnx = exfalso (nnx nx)

-- you have to decide:
{-
Dec : Set → Set
Dec A = A ⊎ ¬ A
-}

open import Lib.Dec.PatternSynonym

ee1 : {X Y : Set} → Dec (X ⊎ Y → ¬ ¬ (Y ⊎ X))
ee1 = yes (λ xory nyorx → case xory (λ x → nyorx (inr x)) λ y → nyorx (inl y))

ee2 : {X : Set} → Dec (¬ (X ⊎ ¬ X))
ee2 = no (λ nxornx → nxornx (inr λ x → nxornx (inl x)))

e3 : {X : Set} → Dec (¬ (X → (¬ X → X)))
e3 = no λ f → f (λ x nx → x)

e4 : Dec ℕ
e4 = yes zero

e5 : Dec ⊥
e5 = no id

e6 : {X : Set} → Dec (⊥ → X ⊎ ¬ X)
e6 = yes (λ bot → exfalso bot)

e7 : {X : Set} → Dec (X × ¬ X → ¬ X ⊎ X)
e7 = yes λ (x , _) → inr x

e8 : {X : Set} → Dec ((X → X) → ⊥)
e8 = no (λ f → f (λ x → x))

f1 : {X Y : Set} → ¬ ¬ X ⊎ ¬ ¬ Y → ¬ ¬ (X ⊎ Y)
f1 (inl nnx) nxory = nxory (exfalso (nnx (λ x → nxory (inl x))))
f1 (inr nny) nxory = nxory (exfalso (nny (λ y → nxory (inr y))))

f2 : ({X Y : Set} → ¬ (X × Y) → ¬ X ⊎ ¬ Y) → {X Y : Set} → ¬ ¬ (X ⊎ Y) → ¬ ¬ X ⊎ ¬ ¬ Y
f2 f {X} {Y} ¬¬x⊎y = f {¬ X} {¬ Y} λ {(¬x , ¬y) → ¬¬x⊎y (λ {(inl x) → ¬x x
                                                          ; (inr y) → ¬y y})}

----------------------------------------------------------------------
-- Not exactly first order logic but kinda is and kinda isn't.

f3 : Dec ((X Y : Set) → X ⊎ Y → Y)
f3 = no λ xory → xory ⊤ ⊥ (inl tt)

f4 : Dec ((X Y Z : Set) → (X → Z) ⊎ (Y → Z) → (X ⊎ Y → Z))
f4 = no λ x → x ⊤ ⊥ ⊥ (inr id) (inl tt)

f5 : Dec ((X Y Z : Set) → (X → Z) × (Y → Z) → (X × Y → Z))
f5 = yes (λ X Y Z f xy → (fst f) (fst xy))

f6 : Dec ((X Y Z : Set) → (X × Y → Z) → (X → Z) × (Y → Z))
f6 = no λ x → (fst (x ⊤ ⊥ ⊥ (λ {(top , bot) → bot}))) tt

f7 : Dec ((X Y Z : Set) → (X ⊎ Y × Z) → (X ⊎ Y) × (X ⊎ Z))
f7 = yes (λ {X Y Z (inl x) → (inl x) , (inl x)
           ; X Y Z (inr (y , z)) → (inr y) , (inr z)})

f8 : Dec ((X Y Z : Set) → (X ⊎ Y) × (X ⊎ Z) → ((X ⊎ Y) × Z))
f8 = no (λ x → snd (x ⊤ ⊤ ⊥ ((inl tt) , (inl tt))))
 
f9 : Dec ((X Y Z : Set) → (X ⊎ Y) × (X ⊎ Z) → (X ⊎ Y × Z))
f9 = yes (λ {X Y Z (inl x , _) → inl x
           ; X Y Z (_ , inl x) → inl x
           ; X Y Z (inr y , inr z) → inr (y , z)})
           