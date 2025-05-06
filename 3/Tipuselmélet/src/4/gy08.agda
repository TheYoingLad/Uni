module gy08 where

open import Lib 
  hiding (sym; cong; subst; idr+; sucr+; assoc+; comm+; dist+*; nullr*; idl*; idr*; sucr*; assoc*; comm*)
  renaming (trans to transₗ)

---------------------------------------------------------
-- equality
------------------------------------------------------

data Eq' (A : Set) : A → A → Set where
  refl : (a : A) → Eq' A a a


sym : ∀{i}{A : Set i}{x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

{-
other notions of trans:
_◾_ = \sq5

\< = ⟨ ; \> = ⟩ ; \qed = ∎
_≡⟨_⟩_ + _∎

_≡⟨_⟩_
Λ  Λ ^-- proof
|  |
|  ⌞ proof
value

_∎ = basically reflexivity proof with an explicit value

Usual way of using this notion:

value1
  ≡⟨ proof1 ⟩
value2
  ≡⟨ proof2 ⟩
value3
  ≡⟨ proof3 ⟩
value4 ∎
-}

_≡⟨_⟩_ : ∀{i}{A : Set i}(x : A){y z : A} → x ≡ y → y ≡ z → x ≡ z
_ ≡⟨ p ⟩ q = transₗ p q

-- \qed = ∎
_∎ : ∀{i}{A : Set i}(x : A) → x ≡ x
_ ∎ = refl

infixr 2 _≡⟨_⟩_
infix 3 _∎


cong : ∀{i j}{A : Set i}{B : Set j}(f : A → B){x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

subst : ∀{i j}{A : Set i}(P : A → Set j){x y : A} → x ≡ y → P x → P y
subst P refl px = px


sym' : ∀{i}{A : Set i}{x y : A} → x ≡ y → y ≡ x
sym' {i} {A} {x} {y} x≡y = subst (λ a → a ≡ x) x≡y refl

cong' : ∀{i j}{A : Set i}{B : Set j}(f : A → B){x y : A} → x ≡ y → f x ≡ f y
cong' {i} {j} {A} {B} f {x} {y} eq = subst (λ a → f x ≡ f a) eq refl

trans' : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans' {i} {A} {x} {y} {z} x≡y y≡z = subst (λ a → x ≡ a) y≡z x≡y

trans'' : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans'' {i} {A} {x} {y} {z} x≡y y≡z = subst (_≡ z) (sym $ x≡y) y≡z

---------------------------------------------------------
-- properties of +,*
---------------------------------------------------------

idl+ : (n : ℕ) → zero + n ≡ n
idl+ n = refl

idr+ : (n : ℕ) → n + zero ≡ n
idr+ zero = refl
idr+ (suc n) = cong suc (idr+ n)

sucr+ : (n m : ℕ) → n + suc m ≡ suc (n + m)
sucr+ zero m = refl
sucr+ (suc n) m = cong suc (sucr+ n m)

ass+ : (m n o : ℕ) → (m + n) + o ≡ m + (n + o)
ass+ zero n o = refl
ass+ (suc m) n o = cong suc (ass+ m n o)

comm+-helper : (n m : ℕ) → suc n + m ≡ n + suc m
comm+-helper zero m = refl
comm+-helper (suc n) m = cong suc (comm+-helper n m)

comm+ : (m n : ℕ) → m + n ≡ n + m
comm+ zero zero = refl
comm+ (suc m) zero = cong suc (idr+ m)
comm+ zero (suc n) = sym $' cong suc (idr+ n)
comm+ (suc m) (suc n) = sym (cong suc (trans (sucr+ n m) (sym (trans (sucr+ m n) (cong suc (comm+ m n))))))

comm+' : (m n : ℕ) → m + n ≡ n + m
comm+' m zero = idr+ m
comm+' m (suc n) = (trans (sucr+ m n) (cong suc (comm+ m n)))

dist+* : (m n o : ℕ) → (n + o) * m ≡ n * m + o * m
dist+* m zero o = refl
dist+* m (suc n) o = sym (trans (ass+ m (n * m) (o * m)) (cong (m +_) (sym (dist+* m n o))))

nullr* : (n : ℕ) → n * 0 ≡ 0
nullr* zero = refl
nullr* (suc n) = nullr* n

idl* : (n : ℕ) → 1 * n ≡ n
idl* n = idr+ n

idr* : (n : ℕ) → n * 1 ≡ n
idr* zero = refl
idr* (suc n) = cong suc (idr* n)

sucr* : (n m : ℕ) → n * suc m ≡ n + n * m
sucr* zero m = refl
sucr* (suc n) m = cong suc (sym (trans (comm+ n (m + n * m)) (trans (ass+ m (n * m) n) (cong (m +_) (trans (comm+ (n * m) n) (sym (sucr* n m)))))))

ass* : (m n o : ℕ) → (m * n) * o ≡ m * (n * o)
ass* zero n o = refl
ass* (suc m) n o = trans (dist+* o n (m * n)) (cong (n * o +_) (ass* m n o))

comm*-helper : (n m : ℕ) → n + n * m ≡ n * suc m
comm*-helper n m = sym (sucr* n m)

comm* : (m n : ℕ) → m * n ≡ n * m
comm* zero n = sym (trans (nullr* n) refl) 
comm* (suc m) n = sym (trans (sucr* n m) (cong (n +_) (sym (comm* m n))))