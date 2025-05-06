<!--
```agda
open import Lib
```
-->
(Előre is bocsi a rövid ü-kért.)

# Elmélet

## 1.) Vezesd le, hogy hány darab eleme van az alábbi típusnak!

Σ ℕ λ n →  (k : ℕ) → (k < n) → Fin n → Fin k

### Megoldás

Nézzük főbb elemei szerint:

```plaintext
Σ ℕ (...)
```

Szigmát úgy bontunk fel, hogy az elsőnek megadott típus minden létező elemét be helyettesítjük a másodiknak megadott típusba. És ezeknek a kifejezéseknek a számosságát összeadjuk.

Másik példával:  

```plaintext
| Σ Bool λ b → if b then ⊤ else Bool | = |if true then ⊤ else Bool| + |if false then ⊤ else Bool| =(Béta szabállyal) |⊤| + |Bool| = 1 + 2 = 3
```

Azonban a rekurzív típusoknál kicsit érdekesebb a helyzet... Van rekurzív típus, aminek az elemszáma véges (Fin n), ezeknél zsintén szét tudjuk választani esetekre, azonban van olyan, aminek nincsen véges elemszáma (ℕ, Lista A), ezeknél a függőtípusoknál nem tudunk megszabadulni. Ezért benne hagyjuk őket, ahogy itt is:

```plaintext
|Σ ℕ (...)| = Σ |(...)| (ahol a sigma megy n = 0-tól végtelenig. Igen, ezt elrontottam órán.)
```

Ergo:
``plaintext
|Σ ℕ λ n →  (k : ℕ) → (k < n) → Fin n → Fin k| = Σ |(k : ℕ) → (k < n) → Fin n → Fin k|

Következő főbb elem:

```plaintext
(k : ℕ) → (...)
```

Agdában a függőtípusos nyílnak, azaz a Pi típusnak nincsen külön jele (más nyelvben, mint pl a Dhall nyelvben, van), ezért ügyelni kell. Ha ilyet látunk hogy `(<név> : <típus>) →` akkor az minden bizonnyal a Pi típs lesz.

Egy másik, egyszerü példával szemléltetve a számosságot:

```plaintext
|(b : Bool) → if b then ℕ else Fin 10| = |Π Bool λ b → if b then ℕ else Fin 10 | = | if true then ℕ else Fin 10 | * |if false then ℕ else Fin 10| =(béta szabállyal) = |ℕ| * |Fin 10| = |ℕ| * 10
```

Tehát vesszük az elsőnek megadott típus minden elemét, beillesztjük a másodiknak megadott típusba ezeket az értékeket. Majd ezek után összeszorozzuk az így kapott kifejezéseket.

Rekurzív típusoknál ismételten nem tudunk nagyon megszabadulni az elsőnek megadott típustól, hiszen annak minden eleme "nem ismert". (lásd: Ha valaki mond nekünk egy természetes számot, mindig tudunk annál nagyobbat megadni).

Tehát:

```plaintext
|(k : ℕ) → (...)| = Π |...|(ahol a sigma megy k = 0-tól végtelenig. Igen, ezt is elrontottam órán, sorry.)
```

Folytassuk tovább a típusunk fejtegetését:

```plaintext
Σ |(k : ℕ) → (k < n) → Fin n → Fin k| = Σ (Π |(k < n) → Fin n → Fin k|)
```

Következő főbb elem:
```plaintext
(k < n)
```

Itt biztosan fent akad a szemünk. Mit kerese egy logikai állítás (az ÉN) típus(om)ban?! Ezeket a logikai állításokat egyszerüen lehet formalizálni: amennyiben igaz az állítás, cseráljük le ⊤-ra, amennyiben hamis, cseréljük le ⊥-ra!

Mj: eredetileg a Lib-ben tényleg így voltak/vannak formalizálva a kifejezések, azonban az új Agda verzió új normalizálási szabályokat vezetett be, aminek hatására az ilyen ügyeskedések eltörtek. Éppen ennek a javítása folyik a háttérben.

tehát a kifejezése:
```plaintext
|(k < n)| = Ha k < n, akkor |⊤|
            különben |⊥|
```

így tudjuk tovább fejteni a típusunkat:
```plaintext
Σ (Π |(k < n) → Fin n → Fin k|) = Σ (Π |(k < n) → Fin n → Fin k|) = Ha k < n, akkor Σ (Π |⊤ → Fin n → Fin k|); különben Σ (Π |⊥ → Fin n → Fin k|)
```

Ilyenkor azonban jöhet az analízisben megszokott mondat: "Vegyük észre, hogy..."  
A Pi típust szét tudjuk szedni kettő szorzatra: a k megy n-1-ig, illetve k megy n-1-től végtelenig. Így a szöveges elágazást át tudjuk alakítani:

```plaintext
Ha k < n, akkor Σ (Π |⊤ → Fin n → Fin k|); különben Σ (Π |⊥ → Fin n → Fin k|) = Σ (Π\^{n-1} (|⊤ → Fin n → Fin k|) * Π\_{k = n-1} (|⊥ → Fin n → Fin k|)) (itt a \-el jelölt karakterek a megszokott LaTeX-es jelölések, azaz legyen a kifjezése a szigma tetején, illetve alján.) 
```

Ezek után alkalmazhatjuk a nyílnak a szabáláyt:
```plaintext
= Σ (Π\^{n-1} (|⊤ → Fin n → Fin k|) * Π\_{k = n-1}(|⊥ → Fin n → Fin k|)) = Σ (Π\^{n-1} (|Fin n → Fin k| ^ |⊤|) * Π\_{k = n-1} (|Fin n → Fin k| ^ |⊥|)) = Σ (Π\^{n-1} (|Fin n → Fin k| ^ 1) * Π\_{k = n-1} (|Fin n → Fin k| ^ 0)) =("általános iskola") Σ (Π\^{n-1} (|Fin n → Fin k|) * Π\_{k = n-1} (1)) = Σ (Π\^{n-1} (|Fin n → Fin k|) * (1)) = Σ (Π\^{n-1} (|Fin n → Fin k|))
```

Végül a kifejezésünk "hasában" a kifejezés áll:
```plaintext
|Fin n → Fin k|
```

A (Fin h) azon típus, aminek az elemszáma pontosan (h), ezt alkalmazva:
```plaintext
|Fin n → Fin k| = |Fin k| ^ |Fin n| = k ^ n
```

Így a végeredményünk:
```plaintext
Σ (Π\^{n-1} (|Fin n → Fin k|)) = Σ (Π\^{n-1} (k ^ n))
```

Itt már meg lehet állni. A lényeg, hogy lássam a lépéseket, amik ide vezetnek.

## 2.) Vegyük az alábbi típust:...

```agda
data X : ℕ → Set where
  X1 : (n k : ℕ) → X n → X (n + k)
  X2 : (n : ℕ) → Fin n → X (suc n)
```
## a.) Írd le az *X* Típus iterátorának típusát, és annak *β*-szabáláyt!

### Megoldás

Valljuk be, a függő típusokkal bánni nem egyszerü. Az iterátor írás ezért sem olyan kézen fekvő. Szerencsére, itt is van pár "ököl szabály".

```agda
iteX : {A : ℕ → Set}{n : ℕ} → ((n k : ℕ) → A n → A (n + k)) → ((n : ℕ) → Fin n → A (suc n)) → X n → A n
--          ^           ^        ^                                  ^ Második konstruktor
--          |           |        | Első konstruktor. "X n" helyén "A n"
--          |           | Ide vegyük fel azt a típusú dolgot, amitől függ az X!
--          | Ide vegyük fel az "X : " utáni rész, mint típus

iteX f g (X1 n k xn) = f n k (iteX f g xn)
iteX f g (X2 n fin ) = g n fin
```

Ehhez hasonlóan megadható más tipusokra az iterátor.

## b.) Írd le az *X* típus *η*-szabáláyt!

### Megoldás

Ez a szabály arra vonatkozik, hogyha tudunk megadni egy függvényt, ami *destruálja* az adott típusunkat (X), és pontosan ugyanúgy viselkedik, mint az `iteX`, akkor az a függvény **biztosan** az `iteX`.

Ezt kell megfogalmaznunk a bépített egyenlőségünkkel.

Erre is tudunk "ököl szabályt" adni.

```agda

etaX : {A : ℕ → Set}{n : ℕ}→(f : (n k : ℕ) → A n → A (n + k))→(g : (n : ℕ) → Fin n → A (suc n))
--^ Másoljuk be az iterátor elejét, és a megadott paramétereket nevezzük el!
  → (ite : {B : ℕ → Set}{h : ℕ} → ((a b : ℕ) → B a → B (a + b)) → ((a : ℕ) → Fin a → B (suc a)) → X h → B h)
--^ Zárójelen belül másoljuk be az egész iterátor típusunkat! nevezzük el valami szépnek.
  → (((a b : ℕ)(xn : X a) → ite f g (X1 a b xn) ≡ f a b (ite f g xn)))
--^ Most jön az "érdekes" rész
-- Propozícionális egyenlőségben (érts: fura egyenlőség/egyenlőség típus) vegyük fel,
-- hogy az "ite" függvény hívás az első konstruktorra
-- mit ad vissza
  → (((a : ℕ)(fin : Fin a) → ite f g (X2 a fin) ≡ g a fin)) → (xn : X n)
--^ A fura egyenlőségel vegyük fel, hogy  mit kéne
-- visszaadnia a függvény hívásának
  → ite f g xn ≡ iteX f g xn
--^ Ha mind ezt megtettük, akkor mondjuk ki, hogy
-- az ite függvény minden "X n" esetére
-- megegyezik az "iteX" függvénnyel

{-
Tipp: Ahol meg kell adni, hogy az "ite" hívása
mit adna vissza, akkor csak másoljuk ki az "iteX"
definícióját, majd végezzük el a megfelelő
változtatásokat!

Példa:

"iteX f g (X1 n k xn) = f n k (iteX f g xn)"
- 1. Írjuk át az "iteX"-et "ite"-re, az "=" pedig
- "≡"-re!

"ite f g (X1 n k xn) ≡ f n k (ite f g xn)"
- 2. Mi hiányzik? A konstruktorhoz a paraméterek!
- Vegyük fel őket a hívás elé!

"(n k : ℕ)(xn : X n) → ite f g (X1 n k xn) ≡ f n k (ite f g xn)"
-3. Profit

-}


-- EZ ALATTA MÁR NEM KELL, CSAK ZSERBÓ BE AKARTA LÁTNI!!!!
etaX f g ite x x₁ (X1 n k xn) = trans (x n k xn) (cong (λ y → f n k y) (etaX f g ite x x₁ xn))
etaX f g ite x x₁ (X2 n x₂) = trans (x₁ n x₂) refl

```

## 3.) Formalizáld...

### Megoldás

Univerzum: Hallgatók
függvények: NINCS (nincsen olyan dolog, ami hallgatóból hallgatóba képezne)
predikátumok:
- a(x) = x átment a ZH-n
- k(x) = x konzultál

a) Minden hallgató átmegy a zh-n: ∀h: a(h)
b) Van olyan hallgató, aki nem megy át a ZH-n vagy konzultál: ∃h: ¬ a(h) ∨ k(h)
c) Van olyan hallgató, aki ha nem konzultál, akkor nem megy át a ZH-n: ∃h: ¬ k(h) ⊃ ¬ a(h)

## 4.) Mit jelent...

MJ: Nagy eséllyel ez a "Joker" kérdés, ami helyére kb bármilyen elmélet mehet.

### Megoldás

Azt jelenti, hogy egy típus konstruktorai egyérelmüen megkülönböztethetőek egymástól. Tehát két különböző konstruktor nem lesz egyenlő.

Tetszőleges típust: Bool ⊎ ⊤

Szabályok:
inl (true) ≢ inl (false)
inl (true) ≢ inr (tt)
inl (false) ≢ inr (tt)

Darab : k alatt a 2.
Indok : Tekintsünk egy típusra úgy, mint egy halmaz, ami a konstruktorait tartalmazza. Ez alapján ebből a *k* elem halmazból ki kell választani 2 különbözőt, amikre belátjuk, hogy nem egyenlőek.


# Gyakorlat

```agda

transpose : {A : Set}{n k : ℕ} → Vec (Vec A n) k → Vec (Vec A k) n
transpose {A} {zero} {zero} x = x
transpose {A} {zero} {suc k} x = []
transpose {A} {suc n} {zero} x = [] ∷ (transpose {A} {n} {zero} [])
transpose {A} {suc n} {suc k} ((x ∷ x₂) ∷ x₁) = (x ∷ Vec.map Vec.head x₁) ∷ transpose (x₂ ∷ (Vec.map Vec.tail x₁))



pr6 : (n : ℕ) → suc (n * suc (suc n)) ≡ (n + suc zero) ^ (suc (suc zero)) + zero
{-
Egyik út
pr6 zero = refl
pr6 (suc n) rewrite idr* (n + (suc zero)) | comm+ n (suc zero) | comm* (n + suc n) (suc (suc n)) | comm+ n (suc (suc (n + n * suc (suc n)))) | comm+ (n + n * suc (suc n)) n = cong suc (trans (cong (λ x → suc (suc (suc (n + x)))) (trans (comm* n (suc (suc (suc n)))) ((sym (cong (n +_) (comm* n (suc (suc n)))))))) (sym (idr+ _)))
-}
pr6 n rewrite idr+ ((n + (suc zero)) * ((n + (suc zero)) * (suc zero))) | idr* (n + (suc zero)) | dist+* n (suc zero) (n + suc zero) | idr+ (n + suc zero) | comm* n (n + suc zero) | dist+* n (suc zero) n | idr+ n | comm+ n (suc zero) | comm* n (suc (suc n)) | sym (assoc+ n n (n * n)) | comm+ (n * n + n) (suc n) | comm+ (n * n) n | sym (assoc+ n n (n * n)) = refl

```

