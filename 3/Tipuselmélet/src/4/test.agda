module test where
open import Lib

f : ℕ → ℕ
f zero = suc(suc(suc(suc(suc(suc(suc zero)))))) 
f (suc x) = f x + suc (suc (suc (zero)))

iso2 : {A B C D : Set} → (A × (B × (C ⊎ D)) ↔ A × B × C ⊎ A × B × D)
fst iso2 (a , b , inl c) = inl (a , b , c)
fst iso2 (a , b , inr d) = inr (a , b , d)
snd iso2 (inl (a , b , c)) = a , b , inl c
snd iso2 (inr (a , b , d)) = a , b , inr d

iso3 : Maybe (⊥ × ⊤ ⊎ Bool) ↔ (Maybe ⊥ × ⊥) ⊎ (⊥ → ⊥) × (Maybe Bool)
fst iso3 nothing = inr (id , nothing)
fst iso3 (just (inr b)) = inr (id , just b)
snd iso3 (inr (bb , nothing)) = nothing
snd iso3 (inr (bb , just b)) = just (inr b)

diffℕ : ℕ → ℕ → ℕ
diffℕ zero zero = zero
diffℕ zero y = y
diffℕ x zero = x
diffℕ (suc x) (suc y) = diffℕ x y

manhattanDistance : ℕ × ℕ → ℕ × ℕ → ℕ
manhattanDistance (a , b) (c , d) = (diffℕ a c) + (diffℕ b d)

xorFilter : {A : Set} → (A → Bool) → (A → Bool) → List A → List A
xorFilter f g [] = []
xorFilter f g (x ∷ xs) = if (f x) xor (g x) then x ∷ (xorFilter f g xs) else xorFilter f g xs

record Machine : Set where
  coinductive
  field
    setℕ    : ℕ → Machine
    getℕ    : ℕ
    applyF  : (ℕ → ℕ) → Machine

open Machine

applier : ℕ → Machine
setℕ (applier x) = applier
getℕ (applier x) = x
applyF (applier x) = λ f → applier (f x)

fib' : ℕ → ℕ → Stream ℕ
head (fib' a b) = a
tail (fib' a b) = fib' b (a + b)

fib : Stream ℕ
fib = fib' 0 1