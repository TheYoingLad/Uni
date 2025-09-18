module Logika where

-- <fgv_nev> :: <tipus> -> <tipus> -> ... -> <tipus> 



konst :: Bool
konst = True

cucc :: Bool -> Bool -> Bool -> Bool
cucc True True True = True
cucc _ b _ = b

not' :: Bool -> Bool
not' True = False
not' False = True

not'' :: Bool -> Bool
not'' True = False
not'' _ = True



and' :: Bool -> Bool -> Bool
and' False False = False
and' True False = False
and' False True = False
and' True True = True

and'' :: Bool -> Bool -> Bool
and'' True True = True
and'' _ _ = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True


myAnd :: Bool -> Bool -> Bool
myAnd True   x  = x
myAnd _      _  = False