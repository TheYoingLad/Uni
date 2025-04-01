module Homework where

for :: a -> (a -> Bool) -> (a -> a) -> b -> (b -> b) -> b
for i p fi x fx = if p i then for (fi i) p fi (fx x) fx else x

markMyWords :: ([Char] -> Bool)  -> [[Char]] -> [[Char]]
markMyWords p = map ('*':) . filter p