{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Assembly where

import Data.Traversable
import Control.Applicative
import Control.Monad
import Data.String
import Data.Maybe
import Data.List
import Debug.Trace

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

data ProgState = ProgState { r1 :: Int, r2 :: Int, r3 :: Int, cmp :: Ordering, memory :: [Int] } deriving (Eq, Show)

startState :: ProgState
startState = ProgState 0 0 0 EQ (replicate 10 0)

type Label = String  -- címke a programban, ahová ugrani lehet

data Register
  = R1
  | R2
  | R3
  deriving (Eq, Show)

data Destination
  = DstReg Register     -- regiszterbe írunk
  | DstDeref Register   -- memóriába írunk, az adott regiszterben tárolt index helyére
  deriving (Eq, Show)

data Source
  = SrcReg Register     -- regiszterből olvasunk
  | SrcDeref Register   -- memóriából olvasunk, az adott regiszterben tárolt index helyéről
  | SrcLit Int          -- szám literál
  deriving (Eq, Show)

data Instruction
  = Mov Destination Source   -- írjuk a Destination-be a Source értékét
  | Add Destination Source   -- adjuk a Destination-höz a Source értékét
  | Mul Destination Source   -- szorozzuk a Destination-t a Source értékével
  | Sub Destination Source   -- vonjuk ki a Destination-ből a Source értékét
  | Cmp Source Source        -- hasonlítsunk össze két Source értéket `compare`-el, az eredményt
                             -- írjuk a `cmp` regiszterbe

  | Jeq Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `EQ` van
  | Jlt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `LT` van
  | Jgt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `GT` van
  deriving (Eq, Show)

type RawProgram = [Either Label Instruction]

-- Beírunk r1-be 10-et, r2-be 20-at
p1 :: RawProgram
p1 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Left "l1",                            -- tehetünk bárhova címkét, nem muszáj használni a programban
  Right $ Mov (DstReg R2) (SrcLit 20)
  ]

-- Kiszámoljuk 10 faktoriálisát, az eredményt r2-ben tároljuk
p2 :: RawProgram
p2 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Right $ Mov (DstReg R2) (SrcLit 1),
  Left "loop",
  Right $ Mul (DstReg R2) (SrcReg R1),
  Right $ Sub (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 0),
  Right $ Jgt "loop"
  ]

-- Feltöltjük 0-9-el a memóriát
p3 :: RawProgram
p3 = [
  Left "start",
  Right $ Mov (DstDeref R1) (SrcReg R1),
  Right $ Add (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 10),
  Right $ Jlt "start"
  ]

-- Megnöveljük 1-el a memória összes mezőjét
p4 :: RawProgram
p4 = [
  Left "start",
  Right $ Add (DstDeref R2) (SrcLit 1),
  Right $ Add (DstReg R2) (SrcLit 1),
  Right $ Cmp (SrcReg R2) (SrcLit 10),
  Right $ Jlt "start"
  ]

-- Kétszer hozzáadunk 1-et a harmadik regiszterhez
p5 :: RawProgram
p5 = [
  Left "start",
  Right $ Jeq "first",
  Left "first",
  Right $ Add (DstReg R3) (SrcLit 1),
  Left "second",
  Right $ Add (DstReg R3) (SrcLit 1)
  ]

type Program = [(Label, [Instruction])]

toProgram :: RawProgram -> Program
toProgram [] = []
toProgram (x:xs) = case x of
    (Right _) -> toProgram xs
    (Left l) -> (l, helper xs) : toProgram xs where
        helper [] = []
        helper (y:ys) = case y of
            (Left _) -> helper ys
            (Right i) -> i:(helper ys)

type M a = State ProgState a

eval :: Program -> [Instruction] -> M ()
eval prog current = do
  (ProgState r1 r2 r3 p mem) <- get 
  case current of
    [] -> return ()
    (ins:rest) -> case ins of
      (Mov dest src) -> do {action dest src (\_ b -> b); eval prog rest}
      (Add dest src) -> do {action dest src (+); eval prog rest}
      (Mul dest src) -> do {action dest src (*); eval prog rest}
      (Sub dest src) -> do {action dest src (-); eval prog rest}
      (Cmp src1 src2) -> do {comp src1 src2; eval prog rest}
      (Jeq l) -> case p of
        EQ -> do {let destCode = fromJust $ lookup l prog in eval prog destCode}
        _ -> eval prog rest
      (Jgt l) -> case p of
        GT -> do {let destCode = fromJust $ lookup l prog in eval prog destCode}
        _ -> eval prog rest
      (Jlt l) -> case p of
        LT -> do {let destCode = fromJust $ lookup l prog in eval prog destCode}
        _ -> eval prog rest

valueOfDest :: Destination -> M Int
valueOfDest dest = case dest of {(DstReg reg) -> valueOfRegister reg; (DstDeref mem) -> valueOfMemory mem}

valueOfSrc :: Source -> M Int
valueOfSrc src = case src of {(SrcReg reg) -> valueOfRegister reg; (SrcDeref mem) -> valueOfMemory mem; (SrcLit n) -> return n}

valueOfRegister :: Register -> M Int
valueOfRegister reg = do
  (ProgState r1 r2 r3 _ _) <- get 
  case reg of {R1 -> return r1; R2 -> return r2; R3 -> return r3}

valueOfMemory :: Register -> M Int
valueOfMemory reg = do
  (ProgState _ _ _ _ mem) <- get 
  val <- valueOfRegister reg
  return (mem!!val)

writeTo :: Destination -> Int -> M ()
writeTo dest n = case dest of {(DstReg reg) -> writeToRegister reg n; (DstDeref mem) -> writeToMemory mem n}

writeToRegister :: Register -> Int -> M ()
writeToRegister reg n = do
  (ProgState r1 r2 r3 comp mem) <- get
  case reg of {R1 -> put $ ProgState n r2 r3 comp mem; R2 -> put $ ProgState r1 n r3 comp mem; R3 -> put $ ProgState r1 r2 n comp mem} 

writeToMemory :: Register -> Int -> M ()
writeToMemory reg n = do
  (ProgState r1 r2 r3 comp mem) <- get
  val <- valueOfRegister reg
  let mem' = [m | ind <- [0..length mem-1], let m = if ind == val then n else mem!!ind]
  put $ ProgState r1 r2 r3 comp mem'

action :: Destination -> Source -> (Int -> Int -> Int) -> M ()
action dest src f = do
  dval <- valueOfDest dest
  sval <- valueOfSrc src
  writeTo dest (f dval sval)

comp :: Source -> Source -> M ()
comp src1 src2 = do
  (ProgState r1 r2 r3 _ mem) <- get
  val1 <- valueOfSrc src1
  val2 <- valueOfSrc src2
  put $ ProgState r1 r2 r3 (compare val1 val2) mem
  
-- futtatunk egy nyers programot a startState-ből kiindulva
runProgram :: RawProgram -> ProgState
runProgram rprog = case toProgram rprog of
  []                  -> startState
  prog@((_, start):_) -> execState (eval prog start) startState