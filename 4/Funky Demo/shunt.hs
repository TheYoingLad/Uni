{-# LANGUAGE LambdaCase #-}

module Shunt where

import Data.List
import Data.Maybe
import Text.Read

-- extra
basicInstances = 0

type OperatorTable a = [(Char, (a -> a -> a, Int, Dir))]

tAdd, tMinus, tMul, tDiv, tPow :: (Floating a) => Tok a
tAdd = TokBinOp (+) '+' 6 InfixL
tMinus = TokBinOp (-) '-' 6 InfixL
tMul = TokBinOp (*) '*' 7 InfixL
tDiv = TokBinOp (/) '/' 7 InfixL
tPow = TokBinOp (**) '^' 8 InfixR

operatorTable :: (Floating a) => OperatorTable a
operatorTable =
  [ ('+', ((+), 6, InfixL)),
    ('-', ((-), 6, InfixL)),
    ('*', ((*), 7, InfixL)),
    ('/', ((/), 7, InfixL)),
    ('^', ((**), 8, InfixR))
  ]

getOp :: (Floating a) => Char -> Maybe (Tok a)
getOp = operatorFromChar operatorTable

parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable

parseAndEval ::
  (String -> Maybe [Tok a]) ->
  ([Tok a] -> ([a], [Tok a])) ->
  String ->
  Maybe ([a], [Tok a])
parseAndEval parse eval input = fmap eval (parse input)

syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))

syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
syEvalPrecedence =
  parseAndEval
    parse
    (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

eqError = 0 -- Ez a tesztelőnek szükséges!

parseAndEvalSafe ::
  (String -> ShuntingYardResult [Tok a]) ->
  ([Tok a] -> ShuntingYardResult ([a], [Tok a])) ->
  String ->
  ShuntingYardResult ([a], [Tok a])
parseAndEvalSafe parse eval input = eval =<< parse input

sySafe :: String -> ShuntingYardResult ([Double], [Tok Double])
sySafe =
  parseAndEvalSafe
    (parseSafe operatorTable)
    (\ts -> shuntingYardSafe (BrckOpen : ts ++ [BrckClose]))

functionTable :: (RealFrac a, Floating a) => FunctionTable a
functionTable =
  [ ("sin", sin),
    ("cos", cos),
    ("log", log),
    ("exp", exp),
    ("sqrt", sqrt),
    ("round", \x -> fromIntegral (round x :: Integer))
  ]

tSin, tCos, tLog, tExp, tSqrt :: (Floating a) => Tok a
tSin = TokFun sin "sin"
tCos = TokFun cos "cos"
tLog = TokFun log "log"
tExp = TokFun exp "exp"
tSqrt = TokFun sqrt "sqrt"

tRound :: (Floating a, RealFrac a) => Tok a
tRound = TokFun (\x -> fromIntegral (round x :: Integer)) "round"

syFun :: String -> Maybe ([Double], [Tok Double])
syFun =
  parseAndEval
    (parseWithFunctions operatorTable functionTable)
    (\t -> shuntingYardWithFunctions $ BrckOpen : (t ++ [BrckClose]))

syComplete :: String -> ShuntingYardResult ([Double], [Tok Double])
syComplete =
  parseAndEvalSafe
    (parseComplete operatorTable functionTable)
    (\ts -> shuntingYardComplete (BrckOpen : ts ++ [BrckClose]))

-- extra over

data Dir = InfixL | InfixR deriving (Show, Eq, Ord)

data Tok a = BrckOpen | BrckClose | TokLit a | TokBinOp (a -> a -> a) Char Int Dir | TokFun (a -> a) String

instance (Eq a) => Eq (Tok a) where
  BrckOpen == BrckOpen = True
  BrckClose == BrckClose = True
  (TokLit a1) == (TokLit a2) = a1 == a2
  (TokBinOp _ c1 i1 d1) == (TokBinOp _ c2 i2 d2) = c1 == c2 && i1 == i2 && d1 == d2
  (TokFun _ s1) == (TokFun _ s2) = s1 == s2
  _ == _ = False

instance (Show a) => Show (Tok a) where
  show BrckOpen = "BrckOpen"
  show BrckClose = "BrckClose"
  show (TokLit a) = "TokLit " ++ show a
  show (TokBinOp _ c i d) = "TokBinOp '" ++ [c] ++ "' " ++ show i ++ " " ++ show d
  show (TokFun _ s) = "TokFun '" ++ s

operatorFromChar :: OperatorTable a -> Char -> Maybe (Tok a)
operatorFromChar table char = case find (\(c, (f, i, d)) -> char == c) table of
  Just (c, (f, i, d)) -> Just $ TokBinOp f c i d
  Nothing -> Nothing

operatorList :: String
operatorList = map fst operatorTable

functionList :: [String]
functionList = map fst functionTable

parseTokens :: (Read a) => OperatorTable a -> String -> Maybe [Tok a]
parseTokens table string =
  foldr
    ( \tok acc -> case acc of
        Nothing -> Nothing
        Just acc -> case tok of
          ['('] -> Just (BrckOpen : acc)
          [')'] -> Just (BrckClose : acc)
          [c] | c `elem` operatorList -> Just (fromJust (operatorFromChar table c) : acc)
          lit ->
            if all (\c -> c == '(' || c == ')') lit
              then Just (map (\c -> if c == '(' then BrckOpen else BrckClose) lit ++ acc)
              else case readMaybe lit of
                Nothing -> Nothing
                Just int -> Just (TokLit int : acc)
    )
    (Just [])
    $ words string

shuntingYardBasic :: [Tok a] -> ([a], [Tok a])
shuntingYardBasic ts = shuntingYardBasicHelper ts ([], [])
  where
    shuntingYardBasicHelper :: [Tok a] -> ([a], [Tok a]) -> ([a], [Tok a])
    shuntingYardBasicHelper [] (lits, toks) = (lits, toks)
    shuntingYardBasicHelper (t : ts) (lits, toks) =
      shuntingYardBasicHelper
        ts
        ( case t of
            BrckOpen -> (lits, BrckOpen : toks)
            (TokLit lit) -> (lit : lits, toks)
            (TokBinOp {}) -> (lits, t : toks)
            BrckClose -> (evalLits evalToks lits, restToks)
              where
                evalToks = takeWhile isBrckOpen toks
                restToks = drop 1 $ dropWhile isBrckOpen toks
                evalLits :: [Tok a] -> [a] -> [a]
                evalLits [] lits = lits
                evalLits ((TokBinOp f _ _ _) : ts) (l2 : l1 : lits) = evalLits ts (f l1 l2 : lits)
        )

getPrecedence :: Tok a -> Int
getPrecedence BrckOpen = 0
getPrecedence (TokBinOp _ _ i _) = i
getPrecedence (TokFun {}) = maxBound

evalLits :: [Tok a] -> [a] -> [a]
evalLits [] lits = lits
evalLits ((TokBinOp f _ _ _) : ts) (l2 : l1 : lits) = evalLits ts (f l1 l2 : lits)
evalLits ((TokFun f _) : ts) (l : lits) = evalLits ts (f l : lits)

isBrckOpen :: Tok a -> Bool
isBrckOpen = \case BrckOpen -> False; _ -> True

shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])
shuntingYardPrecedence ts = shuntingYardPrecedenceHelper ts ([], [])
  where
    shuntingYardPrecedenceHelper :: [Tok a] -> ([a], [Tok a]) -> ([a], [Tok a])
    shuntingYardPrecedenceHelper [] (lits, toks) = (lits, toks)
    shuntingYardPrecedenceHelper (t : ts) (lits, toks) =
      shuntingYardPrecedenceHelper
        ts
        ( case t of
            BrckOpen -> (lits, BrckOpen : toks)
            (TokLit lit) -> (lit : lits, toks)
            (TokBinOp _ _ _ dir) -> (evalLits evalToks lits, t : restToks)
              where
                evalToks = takeWhile isBeforeEval toks
                restToks = dropWhile isBeforeEval toks
                isBeforeEval :: Tok a -> Bool
                isBeforeEval tok = (\p1 p2 -> case dir of InfixL -> p1 >= p2; InfixR -> p1 > p2) (getPrecedence tok) (getPrecedence t)
            BrckClose -> (evalLits evalToks lits, restToks)
              where
                evalToks = takeWhile isBrckOpen toks
                restToks = drop 1 $ dropWhile isBrckOpen toks
        )

data ShuntingYardError
  = OperatorOrClosingParenExpected
  | LiteralOrOpeningParenExpected
  | NoClosingParen
  | NoOpeningParen
  | ParseError
  deriving (Eq, Show)

type ShuntingYardResult a = Either ShuntingYardError a

parseSafe :: (Read a) => OperatorTable a -> String -> ShuntingYardResult [Tok a]
parseSafe table string =
  foldr
    ( \tok acc -> case acc of
        Left _ -> acc
        Right acc -> case tok of
          ['('] -> Right (BrckOpen : acc)
          [')'] -> Right (BrckClose : acc)
          [c] | c `elem` operatorList -> Right (fromJust (operatorFromChar table c) : acc)
          lit ->
            if all (\c -> c == '(' || c == ')') lit
              then Right (map (\c -> if c == '(' then BrckOpen else BrckClose) lit ++ acc)
              else case readMaybe lit of
                Nothing -> Left ParseError
                Just int -> Right (TokLit int : acc)
    )
    (Right [])
    $ words string

hasBrckOpen :: [Tok a] -> Bool
hasBrckOpen = any isBrckOpen

shuntingYardSafe :: [Tok a] -> ShuntingYardResult ([a], [Tok a])
shuntingYardSafe ts = shuntingYardSafeHelper ts (False, Right ([], []))
  where
    shuntingYardSafeHelper :: [Tok a] -> (Bool, ShuntingYardResult ([a], [Tok a])) -> ShuntingYardResult ([a], [Tok a])
    shuntingYardSafeHelper _ (_, err@(Left _)) = err
    shuntingYardSafeHelper [] (_, Right (lits, toks)) =
      if hasBrckOpen toks
        then Left NoClosingParen
        else Right (lits, toks)
    shuntingYardSafeHelper (t : ts) (wasLastLit, Right (lits, toks)) =
      shuntingYardSafeHelper
        ts
        ( case t of
            BrckOpen ->
              if wasLastLit
                then (wasLastLit, Left OperatorOrClosingParenExpected)
                else (False, Right (lits, BrckOpen : toks))
            (TokLit lit) ->
              if wasLastLit
                then (wasLastLit, Left OperatorOrClosingParenExpected)
                else (True, Right (lit : lits, toks))
            (TokBinOp _ _ _ dir) ->
              if wasLastLit
                then (False, Right (evalLits evalToks lits, t : restToks))
                else (wasLastLit, Left LiteralOrOpeningParenExpected)
              where
                evalToks = takeWhile isBeforeEval toks
                restToks = dropWhile isBeforeEval toks
                isBeforeEval :: Tok a -> Bool
                isBeforeEval tok = (\p1 p2 -> case dir of InfixL -> p1 >= p2; InfixR -> p1 > p2) (getPrecedence tok) (getPrecedence t)
            BrckClose ->
              if wasLastLit
                then
                  if hasBrckOpen toks
                    then (True, Right (evalLits evalToks lits, restToks))
                    else (wasLastLit, Left NoOpeningParen)
                else (wasLastLit, Left LiteralOrOpeningParenExpected)
              where
                evalToks = takeWhile isBrckOpen toks
                restToks = drop 1 $ dropWhile isBrckOpen toks
        )

type FunctionTable a = [(String, a -> a)]

functionFromString :: FunctionTable a -> String -> Maybe (Tok a)
functionFromString table str = case find (\(s, f) -> str == s) table of
  Just (s, f) -> Just $ TokFun f s
  Nothing -> Nothing

parseWithFunctions :: (Read a) => OperatorTable a -> FunctionTable a -> String -> Maybe [Tok a]
parseWithFunctions opTable funTable string =
  foldr
    ( \tok acc -> case acc of
        Nothing -> Nothing
        Just acc -> case tok of
          ['('] -> Just (BrckOpen : acc)
          [')'] -> Just (BrckClose : acc)
          [c] | c `elem` operatorList -> Just (fromJust (operatorFromChar opTable c) : acc)
          lit ->
            if all (\c -> c == '(' || c == ')') lit
              then Just (map (\c -> if c == '(' then BrckOpen else BrckClose) lit ++ acc)
              else
                if tok `elem` functionList
                  then Just (fromJust (functionFromString funTable tok) : acc)
                  else case readMaybe lit of
                    Nothing -> Nothing
                    Just int -> Just (TokLit int : acc)
    )
    (Just [])
    $ words string

parseComplete :: (Read a) => OperatorTable a -> FunctionTable a -> String -> ShuntingYardResult [Tok a]
parseComplete opTable funTable string =
  foldr
    ( \tok acc -> case acc of
        Left _ -> acc
        Right acc -> case tok of
          ['('] -> Right (BrckOpen : acc)
          [')'] -> Right (BrckClose : acc)
          [c] | c `elem` operatorList -> Right (fromJust (operatorFromChar opTable c) : acc)
          lit ->
            if all (\c -> c == '(' || c == ')') lit
              then Right (map (\c -> if c == '(' then BrckOpen else BrckClose) lit ++ acc)
              else
                if tok `elem` functionList
                  then Right (fromJust (functionFromString funTable tok) : acc)
                  else case readMaybe lit of
                    Nothing -> Left ParseError
                    Just int -> Right (TokLit int : acc)
    )
    (Right [])
    $ words string

shuntingYardWithFunctions :: [Tok a] -> ([a], [Tok a])
shuntingYardWithFunctions ts = shuntingYardWithFunctionsHelper ts ([], [])
  where
    shuntingYardWithFunctionsHelper :: [Tok a] -> ([a], [Tok a]) -> ([a], [Tok a])
    shuntingYardWithFunctionsHelper [] (lits, toks) = (lits, toks)
    shuntingYardWithFunctionsHelper (t : ts) (lits, toks) =
      shuntingYardWithFunctionsHelper
        ts
        ( case t of
            BrckOpen -> (lits, BrckOpen : toks)
            (TokLit lit) -> (lit : lits, toks)
            (TokFun {}) -> (lits, t : toks)
            (TokBinOp _ _ _ dir) -> (evalLits evalToks lits, t : restToks)
              where
                evalToks = takeWhile isBeforeEval toks
                restToks = dropWhile isBeforeEval toks
                isBeforeEval :: Tok a -> Bool
                isBeforeEval tok = (\p1 p2 -> case dir of InfixL -> p1 >= p2; InfixR -> p1 > p2) (getPrecedence tok) (getPrecedence t)
            BrckClose -> (evalLits evalToks lits, restToks)
              where
                evalToks = takeWhile isBrckOpen toks
                restToks = drop 1 $ dropWhile isBrckOpen toks
        )

shuntingYardComplete :: [Tok a] -> ShuntingYardResult ([a], [Tok a])
shuntingYardComplete ts = shuntingYardCompleteHelper ts (False, Right ([], []))
  where
    shuntingYardCompleteHelper :: [Tok a] -> (Bool, ShuntingYardResult ([a], [Tok a])) -> ShuntingYardResult ([a], [Tok a])
    shuntingYardCompleteHelper _ (_, err@(Left _)) = err
    shuntingYardCompleteHelper [] (_, Right (lits, toks)) =
      if hasBrckOpen toks
        then Left NoClosingParen
        else Right (lits, toks)
    shuntingYardCompleteHelper (t : ts) (wasLastLit, Right (lits, toks)) =
      shuntingYardCompleteHelper
        ts
        ( case t of
            BrckOpen ->
              if wasLastLit
                then (wasLastLit, Left OperatorOrClosingParenExpected)
                else (False, Right (lits, BrckOpen : toks))
            (TokLit lit) ->
              if wasLastLit
                then (wasLastLit, Left OperatorOrClosingParenExpected)
                else (True, Right (lit : lits, toks))
            (TokFun {}) ->
              if wasLastLit
                then (wasLastLit, Left OperatorOrClosingParenExpected)
                else (False, Right (lits, t : toks))
            (TokBinOp _ _ _ dir) ->
              if wasLastLit
                then (False, Right (evalLits evalToks lits, t : restToks))
                else (wasLastLit, Left LiteralOrOpeningParenExpected)
              where
                evalToks = takeWhile isBeforeEval toks
                restToks = dropWhile isBeforeEval toks
                isBeforeEval :: Tok a -> Bool
                isBeforeEval tok = (\p1 p2 -> case dir of InfixL -> p1 >= p2; InfixR -> p1 > p2) (getPrecedence tok) (getPrecedence t)
            BrckClose ->
              if wasLastLit
                then
                  if hasBrckOpen toks
                    then (True, Right (evalLits evalToks lits, restToks))
                    else (wasLastLit, Left NoOpeningParen)
                else (wasLastLit, Left LiteralOrOpeningParenExpected)
              where
                evalToks = takeWhile isBrckOpen toks
                restToks = drop 1 $ dropWhile isBrckOpen toks
        )