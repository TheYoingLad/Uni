{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gyak04 where
import Control.Monad

-- Probléma:
-- Tfh van sok, például Maybe a-ba képző függvényünk:

incrementIfEven :: Integral a => a -> Maybe a
incrementIfEven x
  | even x = Just (x + 1)
  | otherwise = Nothing

combineThrees :: Integral a => (a -> a -> a) -> a -> a -> Maybe a
combineThrees f x y
  | (x + y) `mod` 3 == 0 = Just (f x y)
  | otherwise = Nothing

-- Hogyan tudnánk egy olyan függvényt leírni, ami egy számot kap paraméterül
-- erre meghívja az incrementIfEvent, majd ha az Just-ot ad vissza, annak az eredményét
-- és az eredeti számra alkalmazza a combineThrees függvényt a (*) függvénnyel?
-- Pl.:
-- magicFunction 4 == Just 20 (incrementIfEven 4 == 5, 4 + 5 `mod` 3 == 0, 4 * 5 == 20)
-- magicFunction 3 == Nothing (incrementIfEven 3 == Nothing)
-- magicFunction 2 == Nothing (incrementIfEven 2 == 3, 2 + 3 `mod` 3 /= 0)

magicFunction :: Integral a => a -> Maybe a
magicFunction x = case incrementIfEven x of 
  Nothing -> Nothing
  (Just y) -> combineThrees (*) y x

-- Ez még egy darab Maybe vizsgálatnál annyira nem vészes, de ha sokat kell, elég sok boilerplate kódot vezethet be
-- Az úgynevezett "mellékhatást" (tehát ha egy számítás az eredményen kívül valami mást is csinál, Maybe esetén a művelet elromolhat)
-- Erre a megoldás a Monád típusosztály
{-
:i Monad
type Monad :: (* -> *) -> Constraint
class Functor m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=), return #-}
-}
-- A >>= (ún bind) művelet modellezi egy előző "mellékhatásos" számítás eredményének a felhasználását.
-- Maybe esetén (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--                                   ^ csak akkot fut le ha az első paraméter Just a

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM x = incrementIfEven x >>= \y -> combineThrees (*) x y

-- Így lehet több olyan műveletet komponálni, amelyeknek vannak mellékhatásaik
-- Akinek nem tetszik a >>= irogatás létezik az imperatív stílusú do notáció
{-
do
   x <- y
   a
===
y >>= \x -> a
-}

magicFunctionDo :: Integral a => a -> Maybe a
magicFunctionDo x = do
  y <- incrementIfEven x
  combineThrees (*) x y

readTwoLines :: IO ()
readTwoLines = do
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  print (x + y)

-- Monád példa: IO monád
-- "IO a" egy olyan "a" típusú értéket jelent, amelyhez valami I/O műveletet kell elvégezni, pl konzolról olvasás
{-
getLine :: IO String
putStrLn :: String -> IO ()
                         ^ A 'void' megfelelője imperatív nyelvekből, egy olyan típus amelynek pontosan 1 irreleváns eleme van
readLn :: Read a => IO a
print :: Show a => a -> IO ()
-}
-- Az ilyen ()-ba (ún unitba) visszatérő műveleteknél hasznos a >> művelet
-- m1 >> m2 = m1 >>= \_ -> m2
--                    ^ eredmény irreleváns, csak fusson le

-- Írjunk olyan IO műveleteket do notációval és bindokkal amely
-- a, beolvas két sort és a konkatenációjuk kiírja
-- b, beolvas egy számot és kiírja a négyzetét
-- c, kiírja egy lista összes elemét
-- d, beolvas egy számot minden listaelemhez és azt hozzáadja


readAndConcat :: IO ()
readAndConcat = do
  x <- getLine
  y <- getLine
  print (x ++ y)

readAndConcat' :: IO ()
readAndConcat' = getLine >>= \x -> getLine >>= \y -> print (x ++ y)

readAndSq :: IO ()
readAndSq = do
  x <- readLn :: IO Int
  print (x * x)

readAndSq' :: IO ()
readAndSq' = (readLn :: IO Int) >>= \x -> print (x * x)

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x:xs) = do
  print x
  printAll xs

printAll' :: Show a => [a] -> IO ()
printAll' [] = return ()
printAll' (x:xs) = print x >> printAll xs

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd [] = return []
readAndAdd (x:xs) = do
  y <- readLn
  ys <- readAndAdd xs
  return (x+y : ys)

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' [] = return []
readAndAdd' (x:xs) = readLn >>= \y -> readAndAdd' xs >>= \ys -> return (x+y : ys)

-- Monád példa: Állapotváltozás monád (State monád)
--                          v rekord szintaxis, ekvivalens azzal hogy State (s -> (s,a))
newtype State s a = State { runState :: s -> (a, s) } deriving Functor
-- Ezzel nem foglalkozunk még
instance Applicative (State s) where
  pure = return
  (<*>) = ap
-- "State s a" egy s típusú állapot változását (ez az s -> s rész) és egy "a" eredményt reprezentál
-- Példa:
incrementAndEven :: State Int Bool --     v állapotváltozás eredménye, az eredeti állapot megnövelve 1-el
incrementAndEven = State $ \i -> (even i, i + 1)
--                                ^ eredmény: az állapot páros e

-- Primitív állapotváltozások
-- Eredményül visszaadja a jelenlegi állapotot
get :: State s s
get = State $ \s -> (s,s)

-- Felülírja a jelenlegi állapotot
put :: s -> State s ()
put s = State $ const ((), s)

-- runState :: State s a -> s -> (a,s)
-- lefuttat egy állapotváltozást egy adott kezdeti állapotra

-- Monád műveletek
instance Monad (State s) where

  return :: a -> State s a
  return a = State $ \s -> (a,s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

-- Néha a modify-t is primitívnek szokták mondani
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Minden állapotválotzást megírható >>=, return, get és put segítségével
-- Írjuk meg bindal/do notációval az incrementAndEven állapotváltozást
incrementAndEvenBind :: State Int Bool
incrementAndEvenBind = get >>= \x -> put (x + 1) >> return (even x)

incrementAndEvenDo :: State Int Bool
incrementAndEvenDo = do
  x <- get
  --put (x + 1)
  modify (+1)
  return (even x)

-- runState-el lehet tesztelni, pl runState incrementAndEvenBind 3 == (False, 4)

-- Definiáljunk állapotváltozásokat mely
-- a, leszedi az állapotbeli lista fejelemét ha van olyan
-- b, X darab elemet leszed az állapotbeli listából (csak a primitív kombinátorokat és az a,-t használd)
-- c, addig szedi le az elemekeg az állapotbeli listából, amíg egy predikátum igaz (csak a primitív kombinátorokat és az a,-t használd)
-- d, összeadj az állapotbeli lista összes elemét és kiűríti azt (csak a primitív kombinátorokat és az a,-t használd)

behead :: State [a] (Maybe a)
behead = do
  x <- get
  case x of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

takeFromSt :: Integral i => i -> State [a] [a]
takeFromSt 0 = return []
takeFromSt n = do
  x <- get
  case x of
    [] -> return []
    (a:as) -> do
      put as
      y <- takeFromSt (n-1)
      return (a : y)


takeWhileFromSt :: (a -> Bool) -> State [a] [a]
takeWhileFromSt p = do
  x <- get
  case x of
    [] -> return []
    (a:as) -> if p a then do
      put as
      y <- takeWhileFromSt p
      return (a : y) else return []


summing :: Num a => State [a] a
summing = do
  x <- get
  case x of
    [] -> return 0
    [a] -> do
      put []
      return a
    (a:b:as) -> do
      put ((a+b):as)
      summing

-- Definiáljuk egy fa preorder, postorder és inorder címkézését állapotváltozásokkal
-- Az állapotban a legutoljára kiadott indexet tárolja

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

preorder :: Tree a -> Tree (a, Int)
preorder a = fst $ runState (preorderS a) 0
preorderS :: Tree a -> State Int (Tree (a, Int))
preorderS x = do
  case x of
    (Leaf l) -> do
      i <- get
      modify (+1)
      return (Leaf (l, i))
    (Node t1 n t2) -> do
      i <- get
      modify (+1)
      t1' <- preorderS t1
      t2' <- preorderS t2
      return (Node t1' (n, i) t2')

postorder :: Tree a -> Tree (a, Int)
postorder a = fst $ runState (postorderS a) 0
postorderS :: Tree a -> State Int (Tree (a, Int))
postorderS x = do
  case x of
    (Leaf l) -> do
      i <- get
      modify (+1)
      return (Leaf (l, i))
    (Node t1 n t2) -> do      
      t1' <- postorderS t1
      t2' <- postorderS t2
      i <- get
      modify (+1)
      return (Node t1' (n, i) t2')

inorder :: Tree a -> Tree (a, Int)
inorder a = fst $ runState (inorderS a) 0
inorderS :: Tree a -> State Int (Tree (a, Int))
inorderS x = do
  case x of
    (Leaf l) -> do
      i <- get
      modify (+1)
      return (Leaf (l, i))
    (Node t1 n t2) -> do
      t1' <- inorderS t1
      i <- get
      modify (+1)
      t2' <- inorderS t2
      return (Node t1' (n, i) t2')