module Kiszh where
 
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad

newtype State s a = State { runState :: s -> (s,a) } deriving Functor

instance Applicative (State s) where
    (<*>) = ap
    pure a = State $ \s -> (s,a)

instance Monad (State s) where
    (State g) >>= f = State $ \s -> let (s',  a) = g s in runState (f a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const (s, ())

-- Egyszerű feladat (1 pont)
-- Olvassunk be egy X számot majd olvassunk be X darab számot és adjuk azokat vissza egy listába
-- pl.: readNums
-- > 2
-- > 13
-- > 1
-- [13, 1]

readNs :: Int -> IO [Int]
readNs 0 = return []
readNs x = do 
    y <- readLn :: IO Int
    xs <- readNs (x - 1)
    return (y:xs)

readNums :: IO [Int]
readNums = do
    x <- readLn :: IO Int
    y <- readNs x
    return y