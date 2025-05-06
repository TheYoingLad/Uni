module gy08 where

open import Lib hiding (sym; trans; cong; subst; idr+; sucr+; assoc+; comm+; dist+*; nullr*; idl*; idr*; sucr*; assoc*; comm*)

---------------------------------------------------------
-- equality
------------------------------------------------------

sym : ∀{i}{A : Set i}{x y : A} → x ≡ y → y ≡ x
sym = {!!}

trans : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans = {!!}

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

cong : ∀{i j}{A : Set i}{B : Set j}(f : A → B){x y : A} → x ≡ y → f x ≡ f y
cong = {!!}

subst : ∀{i j}{A : Set i}(P : A → Set j){x y : A} → x ≡ y → P x → P y
subst = {!!}

---------------------------------------------------------
-- properties of +,*
---------------------------------------------------------

idl+ : (n : ℕ) → zero + n ≡ n
idl+ = {!!}

idr+ : (n : ℕ) → n + zero ≡ n
idr+ = {!!}

sucr+ : (n m : ℕ) → n + suc m ≡ suc (n + m)
sucr+ = {!!}

ass+ : (m n o : ℕ) → (m + n) + o ≡ m + (n + o)
ass+ = {!!}

comm+-helper : (n m : ℕ) → suc n + m ≡ n + suc m
comm+-helper = {!!}

comm+ : (m n : ℕ) → m + n ≡ n + m
comm+ = {!!}

dist+* : (m n o : ℕ) → (n + o) * m ≡ n * m + o * m
dist+* = {!!}

nullr* : (n : ℕ) → n * 0 ≡ 0
nullr* = {!!}

idl* : (n : ℕ) → 1 * n ≡ n
idl* = {!!}

idr* : (n : ℕ) → n * 1 ≡ n
idr* = {!!}

sucr* : (n m : ℕ) → n * suc m ≡ n + n * m
sucr* = {!!}

ass* : (m n o : ℕ) → (m * n) * o ≡ m * (n * o)
ass* = {!!}

comm*-helper : (n m : ℕ) → n + n * m ≡ n * suc m
comm*-helper = {!!}

comm* : (m n : ℕ) → m * n ≡ n * m
comm* = {!!}
