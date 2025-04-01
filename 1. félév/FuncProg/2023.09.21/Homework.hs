module Homework where

oneTrue :: Bool -> Bool -> Bool -> Bool
oneTrue True False False = True
oneTrue False True False = True
oneTrue False False True = True
oneTrue _ _ _ = False

isGray :: (Int,Int,Int) -> Bool
isGray (0,0,0) = False
isGray (255,255,255) = False
isGray (x,y,z) = x == y && y == z
isgray _ = False

splitQuadruple :: (a,b,c,d) -> ((a,b),(c,d))
splitQuadruple (x, y, z, w) = ((x,y), (z,w))