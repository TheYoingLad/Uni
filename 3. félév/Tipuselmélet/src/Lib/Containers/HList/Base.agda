{-# OPTIONS --safe --without-K #-}

module Lib.Containers.HList.Base where

open import Lib.Containers.HList.Type
open import Lib.Containers.List.Type
open import Lib.Containers.List.Base using (map)
open import Lib.Level

head : ∀{i}{A : Set i}{As : List (Set i)} → HList (A ∷ As) → A
head (x ∷ _) = x

tail : ∀{i}{A : Set i}{As : List (Set i)} → HList (A ∷ As) → HList As
tail (_ ∷ xs) = xs

{-
mapH : ∀{i j k}{F : Set i → Set j}{A : Set i}{As : List (Set i)} → HList () → HList As → HList (map F As)
mapH f [] = []
mapH {F = F} {A} {B ∷ As} f (x ∷ xs) = {!!}
-}

iteHList : ∀{i}{B : Set i}{As : List (Set i)} → HList (map (λ c → c → B → B) As) → B → HList As → B
iteHList {As = .[]} fs b [] = b
iteHList {As = .(_ ∷ _)} (f ∷ fs) b (x ∷ xs) = f x (iteHList fs b xs)
