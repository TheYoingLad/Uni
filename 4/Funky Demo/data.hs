module MyData where

import Data.Data

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show, Data, Typeable)

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (n `add` m)

mul :: Nat -> Nat -> Nat
mul Zero m = Zero
mul (Succ n) m = n `add` (n `mul` m)

data Bin
  = One
  | Double Bin
  | SuccDouble Bin

fromBin :: Bin -> Integer
fromBin One = 1
fromBin (Double b) = 2 * fromBin b
fromBin (SuccDouble b) = 1 + 2 * fromBin b

data List a
  = Nil
  | Cons a (List a)

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

mirror :: Tree a -> Tree a
mirror Leaf = Leaf
mirror (Node t1 a t2) = Node (mirror t1) a (mirror t2)

data T a
  = T a [T a]