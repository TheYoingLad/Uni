module gy09 where

open import Lib

---------------------------------------------------------
-- equational reasoning
------------------------------------------------------

p4 : (x y : ℕ) → ((x + (y + zero)) + x) ≡ (2 * x + y)
p4 = {!!}

p3 : (a b : ℕ) → a + a + b + a * 0 ≡ 2 * a + b
p3 = {!!}

p2 : (a b c : ℕ) → c * (b + 1 + a) ≡ a * c + b * c + c
p2 = {!!}

p9' : 0 ≢ the ℕ 1
p9' = {!   !}

p9 : 2 * 2 ≢ 5 * 1
p9 = {!   !}

-- Egyszerűbb, amikor mondani kell egy ellenpéldát:
p10 : ¬ ((n : ℕ) → n + 2 ≡ n + 1)
p10 = {!   !}

-- ...mintsem bizonyítani, hogy ez a kettő sosem lesz egyenlő:
p11 : (n : ℕ) → n + 2 ≢ n + 1
p11 = {!   !}

-- Mókásabb helyzet.
p11'' : ¬ Σ ℕ (λ n → n + 3 ≡ n + 1)
p11'' = {!   !}

p12 : ¬ Σ ℕ (λ n → n + n ≡ 3)
p12 = {!   !}

[m+n]^2=m^2+2mn+n^2 : (m n : ℕ) → (m + n) * (m + n) ≡ m * m + 2 * m * n + n * n
[m+n]^2=m^2+2mn+n^2 = {!!}

{-
infixr 8 _^_
_^_ : ℕ → ℕ → ℕ
x ^ zero  = 1
x ^ suc n = x * x ^ n
-}

p1 : (a b : ℕ) → (a + b) ^ 2 ≡ a ^ 2 + 2 * a * b + b ^ 2
p1 = {!!}

0^ : (n : ℕ) → 0 ^ (suc n) ≡ 0
0^ = {!!}

^0 : (a : ℕ) → a ^ 0 ≡ 1
^0 = {!!}

1^ : (n : ℕ) → 1 ^ n ≡ 1
1^ = {!!}

^1 : (a : ℕ) → a ^ 1 ≡ a
^1 = {!!}

^+ : (a m n : ℕ) → a ^ (m + n) ≡ a ^ m * a ^ n
^+ = {!!}

^* : (a m n : ℕ) → a ^ (m * n) ≡ (a ^ m) ^ n
^* = {!!}

*^ : (a b n : ℕ) → (a * b) ^ n ≡ a ^ n * b ^ n
*^ = {!!}
