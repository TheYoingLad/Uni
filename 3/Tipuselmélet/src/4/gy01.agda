module gy01 where

open import Lib

-- 1. git clone https://bitbucket.org/akaposi/ttt
-- 2. Open this file (PATH) in emacs. (On the lab computers: Alt-F2 "emacs")
-- 3. Typecheck with "C-c C-l"
--      -> the file should now be colored

-- Emacs key bindings (C = Ctrl, M = Alt):
--  C-x C-f : create or open a file
--  C-x C-w : save (write) file
--  C-x C-s : save already named file
--  C-x C-c : close Emacs
--  C-space : start selecting text
--  M-w : Copy
--  C-w : Cut
--  C-y : Paste

-- Agda-mode key bindings:
--  C-\       : Switch Agda input mode on/off
--  C-c C-l   : Typecheck the current file
--  C-c C-n   : Normalise an expression (reduce the expression as much as possible)
--  C-c C-d   : Deduce the type of an expression
--  C-c C-,   : Show the goal type and context of a hole
--                (C-u C-u C-c C-, : same but fully normalise the goal
--                 C-u C-c C-,     : same but do not normalise the goal)
--  C-c C-.   : Goal type and context + inferred type of current expression
--  C-c C-SPC : Fill goal
--  C-c C-x = : Describe character at point

-- List of unicode symbols:
--    →       \to
--            \rightarrow
--    ℕ       \bN           'b'lackboard 'N', there is also \bZ for ℤ, etc
--    λ       \Gl           'G'reek 'l', there is also \GG for Γ, etc

{-
Már elvárt ismeret, tárgyhoz előkövetelmények:

Haskell szintaxisa, Haskell nyelv, típusok
  + létező alapvető függvények, operátorok
    + Számokon: _+_, _*_
    + Bool-okon: not, (&&), (||), elágazások
    + Listákon: head, tail, length, replicate, (!!), take, drop, map, filter, zip, zipWith, takeWhile, dropWhile
    + Rendezett párok: (,), fst, snd
    + Either típus: Left, Right
  + operátorok kötési erőssége
  + rekurzió!
  + mintaillesztés!!
    + konstruktorok
      + adatkonstruktorok: Just, Nothing, True, False, [], (:), (,), Left, Right
      + típuskonstruktorok: Maybe, Either, (->)
  + parciális applikálás, pl. (1 +), (+ 2), (+) 3, (`mod` 5), (mod 5)
  + névtelen függvények (lesz róla rövid ismétlés): \x -> x + 1; \x y -> x * 2 - y
  + Típusrendszer:
    + Alapvető típusok jelölése, pl. Int -> Int; a -> b -> a; Eq a => a -> [a] -> Bool
    + Polimorfizmus -> Parametrikus specifikusan; ad-hoc-ról kevés szó lesz.
    + Magasabb rendű függvények jelölése, pl. (a -> b) -> [a] -> [b]; (a -> Bool) -> [a] -> [a]

Fogalmak:
  + paricális/**totális** függvény
  + rekurzió
  + parcális/totális applikálás
  + Curry-zés elve, minden függvény egyparaméteres, \x y -> x + 2 * y ≡ \x -> \y -> x + 2 * y

(Lesz róla szó) Csoport elmélet nagyon eleje: félcsoport, egységelemes félcsoport
(Lesz róla szó, ezeket a tárgy végére kell tudni) Tulajdnoságok: asszociativitás, kommutativitás, egység, disztributivitás
-}

{-
Házi feladatok gyakorlás, nem kötelező.
Nincs +/-
2 db ZH!
40-40 pont
ZH-n kötelező minimum 16 pontot elérni.
80 pont a maximum; a jegyek az alábbiak:

0  - 40.99999... : 1
41 - 49.99999... : 2
50 - 59.99999... : 3
60 - 69.99999... : 4
70 - 80          : 5

1. ZH időpontja: 2024. október  25. péntek 16:30-18:30, 120 perc
2. ZH időpontja: 2024. december 20. péntek 17:00-19:00, 120 perc

A 2 ZH közül CSAK AZ EGYIKET lehet javítani!
Ennek az időpontja kérdéses még; 2025. január 2-10 között valamikor.
-}

-- TODAY:
-- ismétlés; how to haskell in agda
-- learning agda's keycombinations
--  base type ℕ
--  function types   A → B
--   where A and B are any types
--   definitional equality

add3 : ℕ → ℕ
add3 x = x + 3

-- spaces matter!

-- C-c C-n  add3 4

aNum : ℕ
aNum = add3 4

-- equational reasoning
-- aNum = add3 4
--      = 4 + 3
--      = 7

-- DO NOT write brackets in "add3(4)"

-- C-c C-n aNum

bNum : ℕ
bNum = add3 (add3 (add3 2))
bNum' = add3 $' add3 $' add3 2

-- "add3 add3 add3 2" is wrong

-- C-c C-n bNum

-- lambda notation

-- Meaning of defintional equality
add3' : ℕ → ℕ
add3' = λ x → x + 3
-- add3 x = x + 3

-- add3' 4 = (λ x → x + 3) 4
--         = (x + 3)[x := 4]
--         = (4 + 3)
--         = 7

-- test it with C-c C-n!

-- Partial application, just like in haskell (kind of)
add4 : ℕ → ℕ
add4 = 4 +_

-- Goal type and context:             C-c C-,
-- Goal type, context, inferred type: C-c C-.
-- Fill the hole                    : C-c C-space  ,  C-c C-r
-- Creating a hole: enter '?'

-- functions with multiple arguments

add : ℕ → ℕ → ℕ
add = _

-- ℕ → (ℕ → ℕ) = ℕ → ℕ → ℕ
--             ≠ (ℕ → ℕ) → ℕ
-- bracketing of λ

-- same as λ x → λ y → x + y
-- same as λ x y → x + y

add3'' : ℕ → ℕ
add3'' = add 3

num1 : ℕ
num1 = add 3 4

-- bracketing of application

num1' : ℕ
num1' = (add 3) 4

-- what is wrong with the following?

-- num2 : ℕ
-- num2 = add (3 4)

-- what is wrong with the following?

-- num3 : ℕ
-- num3 = add 3 (add 4)

-- compute with equational reasoning:

num4 : ℕ
num4 = add 3 (add 4 2)

-- Higher-order functions: functions with functions as arguments
-- e.g. in Haskell:   map :: (a -> b) -> [a] -> [b]

-- write a function of the following type:

f1 : (ℕ → ℕ) → ℕ
f1 = λ f → f 0

-- test it with f1 add3, f1 add4. is the result the same?

-- write two different functions which use their inputs, i.e.
--   f2 add3 ≠ f2 add4 ≠ f3 add4 ≠ f3 add3

f2 f3 : (ℕ → ℕ) → ℕ
f2 f = f 2
f3 = {!!}



tw : {A : Set} → (A → A) → A → A
tw f n = f (f n)

-- consider

t = tw tw add3 1
-- what is the type of this and why? ask Agda too (C-c C-d).
-- what is its value?  guess, and ask Agda too (C-c C-n).

first : {A : Set} → A → A → A
first = {!!}

second : {A : Set} → A → A → A
second = {!!}

----------------------------------------------

-- Show definitional equality meaning on Bools:
-- constTrue with pattern matching
-- constTrue normally
-- C-c C-n try (λ x → constTrue x), see what happens in each case!

constTrue : Bool → Bool
constTrue _ = true

constTrue' : Bool → Bool
constTrue' true = true
constTrue' false = true
