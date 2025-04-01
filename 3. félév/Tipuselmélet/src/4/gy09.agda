module gy09 where

open import Lib

_≡⟨_⟩_ : ∀{i}{A : Set i}(x : A){y z : A} → x ≡ y → y ≡ z → x ≡ z
_ ≡⟨ p ⟩ q = trans p q

-- \qed = ∎
_∎ : ∀{i}{A : Set i}(x : A) → x ≡ x
_ ∎ = refl

---------------------------------------------------------
-- equational reasoning
------------------------------------------------------

p4 : (x y : ℕ) → ((x + (y + zero)) + x) ≡ (2 * x + y)
p4 x y = 
    (x + (y + zero) + x) 
        ≡⟨ (cong (λ z → x + z + x) (idr+ y)) ⟩ 
    ((x + y + x) 
        ≡⟨ assoc+ x y x ⟩ 
    ((x + (y + x)) 
        ≡⟨ (cong (x +_) (comm+ y x)) ⟩ 
    ((x + (x + y)) 
        ≡⟨ (cong  (λ z → x + (z + y)) (sym (idr+ x))) ⟩ 
    ((x + (x + zero + y)) 
        ≡⟨ (sym (assoc+ x (x + zero) y)) ⟩ 
    ((x + (x + zero) + y) ∎)))))

p4' : (x y : ℕ) → ((x + (y + zero)) + x) ≡ (2 * x + y)
p4' x y = 
    trans (cong (λ z → x + z + x) (idr+ y)) 
    (trans (comm+ (x + y) x) 
    (trans (sym $ assoc+ x x y) 
    (sym 
    (cong (λ z → x + z + y) (idr+ x)))))

--(x y : ℕ) → x + (y + zero) + x ≡ x + (x + zero) + y

p3 : (a b : ℕ) → a + a + b + a * 0 ≡ 2 * a + b
p3 a b = 
    trans (cong (λ c → a + a + b + c) (nullr* a)) 
    (trans (idr+ (a + a + b)) 
    (sym 
    (cong (λ c → a + c + b) (idr+ a))))

p2 : (a b c : ℕ) → c * (b + 1 + a) ≡ a * c + b * c + c
p2 a b c = 
    trans (dist*+ c (b + 1) a) 
    (trans (cong (λ d → d + c * a) (dist*+ c b 1)) 
    (trans (cong (λ d → c * b + d + c * a) (idr* c)) 
    (trans (cong (λ d → d + c + c * a ) (comm* c b)) 
    (trans (comm+ (b * c + c) (c * a)) 
    (trans (sym $ assoc+ (c * a) (b * c) c) 
    (cong (λ d → d + (b * c) + c) (comm* c a)))))))

p9' : 0 ≢ the ℕ 1
p9' ()

p9 : 2 * 2 ≢ 5 * 1
p9 x = subst prov x tt where
    prov : ℕ → Set
    prov (suc (suc (suc (suc zero)))) = ⊤
    prov (suc (suc (suc (suc (suc n))))) = ⊥
    prov _ = ℕ

-- Egyszerűbb, amikor mondani kell egy ellenpéldát:
p10 : ¬ ((n : ℕ) → n + 2 ≡ n + 1)
p10 f with f 0
... | ()

-- ...mintsem bizonyítani, hogy ez a kettő sosem lesz egyenlő:
p11 : (n : ℕ) → n + 2 ≢ n + 1
p11 (suc n) biz = p11 n (cong pred' biz)

-- Mókásabb helyzet.
p11'' : ¬ Σ ℕ (λ n → n + 3 ≡ n + 1)
p11'' (x , biz) = p11h x biz where
    p11h : (n : ℕ) → n + 3 ≢ n + 1
    p11h (suc x) biz = p11h x (cong pred' biz)

p12 : ¬ Σ ℕ (λ n → n + n ≡ 3)
p12 (suc (suc (suc zero)) , ())
p12 (suc (suc (suc (suc x))) , ())

[m+n]^2=m^2+2mn+n^2 : (m n : ℕ) → (m + n) * (m + n) ≡ m * m + 2 * m * n + n * n --átalakítás rekurzív esetre
[m+n]^2=m^2+2mn+n^2 zero n = refl
[m+n]^2=m^2+2mn+n^2 (suc m) n = 
    cong suc 
    (trans (cong (λ o → m + n + o) (comm* (m + n) (suc (m + n)))) 
    (trans (cong (λ o → m + n + o) (assoc+ m n ((m + n) * (m + n)))) 
    (trans (assoc+ m n (m + (n + (m + n) * (m + n)))) 
    (sym 
    (trans (assoc+ (m + m * suc m) (n + (m + suc (m + zero)) * n) (n * n)) 
    (trans (assoc+ m (m * suc m) (n + (m + suc (m + zero)) * n + n * n)) 
    (cong (m +_) 
    (trans (cong (λ o → m * suc m + (n + o * n + n * n)) (comm+ m (suc (m + zero)))) 
    (trans (comm+ (m * suc m) (n + (n + (m + zero + m) * n) + n * n)) 
    (trans (assoc+ (n + (n + (m + zero + m) * n)) (n * n) (m * suc m)) 
    (trans (assoc+ n (n + (m + zero + m) * n) (n * n + m * suc m)) 
    (cong (n +_) 
    (trans (comm+ (n + (m + zero + m) * n) (n * n + m * suc m)) 
    (trans (cong (λ o → n * n + o + (n + (m + zero + m) * n)) (comm* m (suc m))) 
    (trans (cong (λ o → o + (n + (m + zero + m) * n)) (comm+ (n * n) (m + m * m))) 
    (trans (assoc+ (m + m * m) (n * n) (n + (m + zero + m) * n)) 
    (trans (assoc+ m (m * m) (n * n + (n + (m + zero + m) * n))) 
    (cong (m +_) 
    (trans (comm+ (m * m) (n * n + (n + (m + zero + m) * n))) 
    (trans (cong (λ o → o + m * m)(comm+ (n * n) (n + (m + zero + m) * n))) 
    (trans (assoc+ (n + (m + zero + m) * n) (n * n) (m * m)) 
    (trans (assoc+ n ((m + zero + m) * n) (n * n + m * m)) 
    (cong (n +_) 
    (trans (sym $ assoc+ ((m + zero + m) * n) (n * n) (m * m)) 
    (trans (comm+ ((m + zero + m) * n + n * n) (m * m)) 
    (trans (cong (λ o → m * m + (o * n + n * n)) (comm+ (m + zero) m)) 
    (trans (sym $ assoc+ (m * m) ((m + (m + zero)) * n) (n * n)) 
    (sym ([m+n]^2=m^2+2mn+n^2 m n)))))))))))))))))))))))))))))

[m+n]^2=m^2+2mn+n^2' : (m n : ℕ) → (m + n) * (m + n) ≡ m * m + 2 * m * n + n * n --átalakítás megegyezőre
[m+n]^2=m^2+2mn+n^2' m n = 
    trans (dist+* m n (m + n)) 
    (trans (cong (λ o → o + n * (m + n))(dist*+ m m n)) 
    (trans (cong (λ o → m * m + m * n + o) (comm* n (m + n))) 
    (trans (cong (λ o → m * m + m * n + o) (dist+* m n n)) 
    (trans (sym $ assoc+ (m * m + m * n) (m * n) (n * n)) 
    (sym 
    (trans (cong (λ o → m * m + (m + o) * n + n * n) (idr+ m)) 
    (trans (cong (λ o → m * m + o + n * n) (dist+* m m n)) 
    (cong (λ o → o + n * n) 
    (sym $ assoc+ (m * m) (m * n) (m * n))))))))))

{-
infixr 8 _^_
_^_ : ℕ → ℕ → ℕ
x ^ zero  = 1
x ^ suc n = x * x ^ n
-}

p1 : (a b : ℕ) → (a + b) ^ 2 ≡ a ^ 2 + 2 * a * b + b ^ 2
p1 a b = 
    trans (cong (λ c → (a + b) * c) (idr* (a + b))) 
    (sym 
    (trans (cong (λ c → a * c + (a + (a + zero)) * b + b * (b * 1)) (idr* a)) 
    (trans (cong (λ c → a * a + (a + (a + zero)) * b + b * c) (idr* b)) 
    (sym $ [m+n]^2=m^2+2mn+n^2' a b))))

0^ : (n : ℕ) → 0 ^ (suc n) ≡ 0
0^ n = refl

^0 : (a : ℕ) → a ^ 0 ≡ 1
^0 a = refl

1^ : (n : ℕ) → 1 ^ n ≡ 1
1^ zero = refl
1^ (suc n) = trans (idr+ (1 ^ n)) (1^ n)

^1 : (a : ℕ) → a ^ 1 ≡ a
^1 a = idr* a

^+ : (a m n : ℕ) → a ^ (m + n) ≡ a ^ m * a ^ n
^+ a zero n = sym (idr+ (a ^ n))
^+ a (suc m) n = 
    sym 
    (trans (assoc* a (a ^ m) (a ^ n)) 
    (cong (a *_) (sym $ ^+ a m n)))

^* : (a m n : ℕ) → a ^ (m * n) ≡ (a ^ m) ^ n
^* a m zero = trans (cong (λ o → a ^ o) (nullr* m)) refl
^* a m (suc n) = 
    trans (cong (λ o → a ^ o) (comm* m (suc n))) 
    (trans (^+ a m (n * m)) 
    (cong (a ^ m *_) 
    (trans (cong (λ o → a ^ o) (comm* n m)) (^* a m n))))

*^ : (a b n : ℕ) → (a * b) ^ n ≡ a ^ n * b ^ n
*^ a b zero = refl 
*^ a b (suc n) = 
    trans (assoc* a b ((a * b) ^ n)) 
    (sym 
    (trans (assoc* a (a ^ n) (b * b ^ n)) 
    (cong (a *_) 
    (trans (comm* (a ^ n) (b * b ^ n)) 
    (trans (assoc*  b (b ^ n) (a ^ n)) 
    (cong (b *_)  
    (trans (comm*  (b ^ n) (a ^ n)) (sym $ *^ a b n)))))))) 

and : Bool → Bool → Bool
and = λ x y → if x then (if y then true else false) else (if y then false else false)