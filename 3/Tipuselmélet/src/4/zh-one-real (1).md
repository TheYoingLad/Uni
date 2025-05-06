# Első zárthelyi

## 1. feladat

Írjuk meg rekurzió, `suc` és `zero` kizárólagos használatával azt a függvényt amely a `f(x) = 3*x + 7` képlet szerint van definiálva.

```haskell
f : ℕ → ℕ
f = ?
```

## 2. feladat

Bizonyítsd az alábbi izomorfizmust! Mivel izomorfaknak kell lenniük, így szükséges minden különböző elemet különböző elemhez rendelni úgy, hogy oda-vissza és vissza-oda elvégezve a műveletet ugyanazt az értéket kapjuk vissza.

Teljesen mindegy, hogy hogyan van implementálva, hogy mit csinál; a lényeg, hogy oda-vissza alakítva az értékeket ugyanazt kapjuk vissza.

```haskell
iso2 : {A B C D : Set} → (A × (B × (C ⊎ D)) ↔ A × B × C ⊎ A × B × D)
iso2 = ?
```

## 3. feladat

Bizonyítsd az alábbi izomorfizmust! Mivel izomorfaknak kell lenniük, így szükséges minden különböző elemet különböző elemhez rendelni úgy, hogy oda-vissza és vissza-oda elvégezve a műveletet ugyanazt az értéket kapjuk vissza.

Teljesen mindegy, hogy hogyan van implementálva, hogy mit csinál; a lényeg, hogy oda-vissza alakítva az értékeket ugyanazt kapjuk vissza.

```haskell
iso3 : Maybe (⊥ × ⊤ ⊎ Bool) ↔ (Maybe ⊥ × ⊥) ⊎ (⊥ → ⊥) × (Maybe Bool)
iso3 = ?
```

## 4-5. feladat

A `manhattanDistance` függvény két pont (ℕ × ℕ) közti Manhattan-távolságot számolja ki. 

A Manhattan-távolság a következőképp van definiálva : `manhattanDistance (a , b) (c , d) = |a - c| + |b - d|` (vagyis a koordináták távolságának összege).

Írjuk meg a `diffℕ` függvényt, amely két pont különbségének az abszolút értékét számolja ki, majd a `diffℕ`-t felhasználva írjuk meg a `manhattanDistance`-t.

```haskell
diffℕ : ℕ → ℕ → ℕ
diffℕ = ?

manhattanDistance : ℕ × ℕ → ℕ × ℕ → ℕ
manhattanDistance = ?
```

## 6. feladat

Írjuk meg `xorFilter` függvényt listákra. A `xorFilter` kettő predikátumot és egy listát kap paraméterül, majd azokat a lista elemek tartja meg, amelyekre pontosan az egyik predikátum igaz (exclusive or). 

```haskell
xorFilter : {A : Set} → (A → Bool) → (A → Bool) → List A → List A
xorFilter = ?
```

## 7. feladat

Adott egy koinduktív gép az alábbi módon:

- A gépnek átadhatunk egy számot.
- A gép tárol egy természetes számot.
- A gépnek átadhatunk egy ℕ → ℕ függvényt.

```haskell
record Machine : Set where
  coinductive
  field
    setℕ    : ℕ → Machine
    getℕ    : ℕ
    applyF  : (ℕ → ℕ) → Machine

open Machine
```

Definiáljuk az `applier` függvényt, amely felépíti a következő gépet:

- Be lehet állítani a tárolt számot. (setℕ)
- Le lehet kérdezni a tárolt számot. (getℕ)
- Alkalmazni lehet egy általunk megadott függvényt a tárolt számra. (applyF)

```hs
applier : ℕ → Machine
applier = ?
```

## 8. feladat

Írjuk meg a Fibonacci-számokat tartalmazó Stream-et.

A Fibonacci-számok : 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ...

Amelyeket a következő egyenlet definiál:

```hs
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

*Segítség*: Ehhez definiálhatjuk a `fib'` segédfüggvényt, amely két tetszőleges értéktől kezdve olyan `Stream`-et generál, amelynek az első két értéke a két kezdőérték, majd minden `n.` eleme az előző kettőnek az összege.


```haskell
fib' : ℕ → ℕ → Stream ℕ
fib' = ?

fib : Stream ℕ
fib = ?
```
