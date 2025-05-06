-- Ha komintaillesztéskor valakit zavar, hogy a végére teszi az illesztést,
-- az vegye ki a kommentből az ez alatt lévő sort.
-- {-# OPTIONS --no-postfix-projections #-}

module ZH1 where

open import Lib

-- A listás és stream-es függvények eléréséhez
-- vagy a List.<függvény>, Stream.<függvény> kvalifikálást kell használni,
-- vagy pedig open-elni kell ezen modulokat. Vigyázat, a második esetén ütköző
-- nevű függvények elő fognak fordulni átnevezések nélkül.

{-
data CrazyType (A B : Set) : Set where
  C1 : A → CrazyType A B → CrazyType A B
  C2 : ℕ → B → CrazyType A B

iteCrazyType : {A B C : Set} → (A → C → C) → (ℕ → B → C) → CrazyType A B → C
iteCrazyType cc1 cc2 (C1 a ct) = cc1 a (iteCrazyType cc1 cc2 ct)
iteCrazyType cc1 cc2 (C2 n b) = cc2 n b

record Gep : Set where
  coinductive
  field
    give : ℕ → Gep
    push1 : Gep
    push2 : Gep
    get : ℕ

a : Gep
a .Gep.give x = a
a .Gep.push1 = a
a .Gep.push2 = a
a .Gep.get = 2
-}

bij : {A B : Set} → ((Bool → A) × (⊥ → B)) ↔ (A × (A ⊎ (⊥ × B)))
bij .fst f .fst = fst f true
bij .fst f .snd = inl (fst f false)
bij .snd (a₁ , inl a₂) .fst b = if b then a₁ else a₂
bij .snd (a₁ , inl a₂) .snd bot = exfalso bot


bij-test1 : fst (bij {_} {ℕ}) (snd bij (false , inl true)) ≡ (false , inl true)
bij-test1 = refl

bij-test2 : fst (bij {_} {ℕ}) (snd bij (suc zero , inl zero)) ≡ (suc zero , inl zero)
bij-test2 = refl

bij-test3 : fst (snd (bij {_} {Bool}) (fst bij (not , λ ()))) false ≡ true
bij-test3 = refl

bij-test4 : fst (snd (bij {_} {ℕ}) (fst bij ((λ b → if b then suc zero else zero) , λ ()))) false ≡ zero
bij-test4 = refl

bij-test5 : fst (snd (bij {_} {ℕ}) (fst bij ((λ b → if b then suc zero else zero) , λ ()))) true ≡ suc zero
bij-test5 = refl

---------------------------------

record Machine : Set where
  coinductive
  field
    put : Bool → Machine
    get : Bool

open Machine

and : Bool → Bool → Bool
and true x = x
and false x = false

neverAndingMachine : Bool → Machine
neverAndingMachine x .put b = neverAndingMachine (and x b)
neverAndingMachine x .get = x

neverAndingMachine-test1 : get (put (put (put (neverAndingMachine false) false) false) true) ≡ false
neverAndingMachine-test1 = refl

neverAndingMachine-test2 : get (put (neverAndingMachine true) true) ≡ true
neverAndingMachine-test2 = refl

neverAndingMachine-test3 : get (put (put (neverAndingMachine true) true) true) ≡ true
neverAndingMachine-test3 = refl

neverAndingMachine-test4 : get (put (put (put (neverAndingMachine true) true) true) true) ≡ true
neverAndingMachine-test4 = refl

neverAndingMachine-test5 : get (put (put (put (neverAndingMachine true) false) false) true) ≡ false
neverAndingMachine-test5 = refl

neverAndingMachine-test6 : get (put (put (put (neverAndingMachine true) false) true) false) ≡ false
neverAndingMachine-test6 = refl

neverAndingMachine-test7 : get (put (put (put (neverAndingMachine true) true) true) false) ≡ false
neverAndingMachine-test7 = refl

---------------------------------

f8 : ℕ → ℕ
f8 zero = suc zero
f8 (suc n) = suc (suc (suc (f8 n)))

f8-test1 : f8 zero ≡ suc zero
f8-test1 = refl

f8-test2 : {- f8 3 ≡ 10 -} f8 (suc (suc (suc zero))) ≡ suc (suc (suc (suc (suc (suc (suc (suc (suc (suc zero)))))))))
f8-test2 = refl

f8-test3 : {- f8 2 ≡ 7 -} f8 (suc (suc zero)) ≡ suc (suc (suc (suc (suc (suc (suc zero))))))
f8-test3 = refl

f8-test4 : {- f8 5 ≡ 16 -} f8 (suc (suc (suc (suc (suc zero))))) ≡ suc (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc (suc zero)))))))))))))))
f8-test4 = refl

---------------------------------

{-
inits' (reverse (2 :: 1 :: 0 :: []))
inits' (2 :: (1 :: 0 :: []))
(2 :: 1 :: 0 :: []) :: (inits' (1 :: (0 :: [])))
(2 :: 1 :: 0 :: []) :: (1 :: 0 :: []) :: (inits' (0 :: ([])))
(2 :: 1 :: 0 :: []) :: (1 :: 0 :: []) :: (0 :: []) :: (inits' [])
(2 :: 1 :: 0 :: []) :: (1 :: 0 :: []) :: (0 :: []) :: []            | reverse
(0 :: []) :: (1 :: 0 :: []) :: (2 :: 1 :: 0 :: []) :: []            | map (reverse)
(0 :: []) :: (0 :: 1 :: []) :: (0 :: 1 :: 2 :: []) :: []            | [] ::
[] :: (0 :: []) :: (0 :: 1 :: []) :: (0 :: 1 :: 2 :: []) :: []      | done
-}

inits' : {A : Set} → List A → List (List A)
inits' [] = []
inits' (x ∷ xs) = (x ∷ xs) ∷ (inits' xs)

inits : {A : Set} → List A → List (List A)
inits xs = Lib.List.map (Lib.List.reverse) ([] ∷ (Lib.List.reverse (inits' (Lib.List.reverse xs)))) 

--            inits (0 ∷ 1 ∷ 2 ∷ []) ≡ [] ∷ (0 ∷ []) ∷ (0 ∷ 1 ∷ []) ∷ (0 ∷ 1 ∷ 2 ∷ []) ∷ []
inits-test1 : inits (zero ∷ suc zero ∷ suc (suc zero) ∷ []) ≡ [] ∷ (zero ∷ []) ∷ (zero ∷ suc zero ∷ []) ∷ (zero ∷ suc zero ∷ suc (suc zero) ∷ []) ∷ []
inits-test1 = refl

inits-test2 : inits (true ∷ []) ≡ [] ∷ (true ∷ []) ∷ []
inits-test2 = refl

inits-test3 : inits (false ∷ true ∷ false ∷ true ∷ []) ≡ [] ∷ (false ∷ []) ∷ (false ∷ true ∷ []) ∷ (false ∷ true ∷ false ∷ []) ∷ (false ∷ true ∷ false ∷ true ∷ []) ∷ []
inits-test3 = refl

inits-test4 : inits (tt ∷ tt ∷ []) ≡ [] ∷ (tt ∷ []) ∷ (tt ∷ tt ∷ []) ∷ []
inits-test4 = refl

---------------------------------

unzip : {A B : Set} → Stream (A × B) → (Stream A × Stream B)
unzip x .fst .head = fst (head x)
unzip x .fst .tail = fst (unzip (tail x))
unzip x .snd .head = snd (head x)
unzip x .snd .tail = snd (unzip (tail x))

unzip-test1 : (λ (a , b) → (head a , head b)) (unzip (Stream.iterate (λ (a , b) → (suc a , not b)) (zero , false))) ≡ (zero , false)
unzip-test1 = refl

unzip-test2 : (λ (a , b) → (head (tail a) , head (tail b))) (unzip (Stream.iterate (λ (a , b) → (suc a , not b)) (zero , false))) ≡ (suc zero , true)
unzip-test2 = refl

unzip-test3 : (λ (a , b) → (head (tail a) , head (tail (tail (tail b))))) (unzip (Stream.iterate (λ (a , b) → (a , not b)) (zero , false))) ≡ (zero , true)
unzip-test3 = refl

unzip-test4 : (λ (a , b) → (head (tail (tail (tail a)))) , head (tail (tail (tail (tail (tail (tail b))))))) (unzip (Stream.iterate (λ (a , b) → (suc a , pred' b)) (zero , suc (suc (suc (suc (suc (suc (suc zero))))))))) ≡ (suc (suc (suc zero)) , suc zero)
unzip-test4 = refl
      