{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Vizsga where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.List
import Data.Bifunctor
import Data.Bitraversable
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable hiding ( asum )
import GHC.Stack
import Debug.Trace
import Data.Semigroup ( Sum(..), Product(..) )

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use lambda-case" #-}


-- FELADATOK

data JaggedList a = JCons [a] (JaggedList a) | JNil
  deriving (Eq, Show)

infixr 5 `JCons`

instance Functor JaggedList where
    fmap _ JNil = JNil
    fmap f (JCons as js) = JCons (map f as) (fmap f js)

instance Foldable JaggedList where
    foldMap f JNil = mempty
    foldMap f (JCons as js) = foldMap f as <> foldMap f js

instance Traversable JaggedList where
    traverse _ JNil = pure JNil
    traverse f (JCons as js) = JCons <$> traverse f as <*> traverse f js

j1 :: JaggedList Int
j1 = [1,2,3] `JCons` [4,5,6] `JCons` JNil

j2 :: JaggedList Char
j2 = "alma" `JCons` "barack" `JCons` "kókuszdió" `JCons` JNil

j3 :: JaggedList Int
j3 = JCons [1] j3

testGroup1 :: [Bool]
testGroup1 = [
  fmap (*2) j1 ==  [2,4,6] `JCons` [8,10,12] `JCons` JNil,
  fmap (const "text") j2 == ["text","text","text","text"] `JCons` ["text","text","text","text","text","text"] `JCons` ["text","text","text","text","text","text","text","text","text"] `JCons` JNil,
  sum j1 == 21,
  not $ all (>1) j3,
  sequenceA (fmap (\x -> if x < 6 then Just x else Nothing) j1) == Nothing,
  traverse (\x -> if x < 7 then Just x else Nothing) j1 == Just j1,
  evalState (traverse (\x -> if odd x then get >>= \n -> Just (n,x) <$ put (n + 1) else pure Nothing) j1) 0 == [Just (0,1),Nothing,Just (1,3)] `JCons` [Nothing,Just (2,5),Nothing] `JCons` JNil
             ]

longestColumn :: JaggedList a -> Int
longestColumn JNil = 0
longestColumn (JCons as js) = max (length as) $ longestColumn js

testGroup2 :: [Bool]
testGroup2 = [
  longestColumn j1 == 3,
  longestColumn j2 == 9,
  longestColumn JNil == 0,
  longestColumn ([1..10] `JCons` [] `JCons` JNil) == 10
             ]

mapOverColumns :: ([a] -> [b]) -> JaggedList a -> JaggedList b
mapOverColumns _ JNil = JNil
mapOverColumns f (JCons as js) = JCons (f as) $ mapOverColumns f js

testGroup3 :: [Bool]
testGroup3 = [
  mapOverColumns (map (+1)) j1 == fmap (+1) j1,
  mapOverColumns (\xs -> case xs of { [] -> [1..10]; (y:ys) -> [y]; }) ([] `JCons` j1) == [1,2,3,4,5,6,7,8,9,10] `JCons` [1] `JCons` [4] `JCons` JNil,
  mapOverColumns (concatMap (\x -> replicate x x)) j1 == [1,2,2,3,3,3] `JCons` [4,4,4,4,5,5,5,5,5,6,6,6,6,6,6] `JCons` JNil
             ]

matricize :: Monoid a => JaggedList a -> JaggedList a
matricize js = mapOverColumns (\as -> as ++ (replicate (longestColumn js - length as) mempty)) js where

testGroup4 :: [Bool]
testGroup4 = [
  matricize (JCons [] $ JCons ["a", "b", "c"] JNil) == JCons ["","",""] (JCons ["a","b","c"] JNil),
  matricize (() <$ j2) == [(),(),(),(),(),(),(),(),()] `JCons` [(),(),(),(),(),(),(),(),()] `JCons` [(),(),(),(),(),(),(),(),()] `JCons` JNil,
  fmap getSum (matricize $ Sum <$> (JCons [1..5] j1)) == [1,2,3,4,5] `JCons` [1,2,3,0,0] `JCons` [4,5,6,0,0] `JCons` JNil,
  fmap getProduct (matricize $ Product <$> (JCons [1..5] j1)) == [1,2,3,4,5] `JCons` [1,2,3,1,1] `JCons` [4,5,6,1,1] `JCons` JNil,
  fmap getSum (matricize $ Sum <$> (JCons [] j1)) == [0,0,0] `JCons` [1,2,3] `JCons` [4,5,6] `JCons` JNil
             ]

prettyPrint :: Show a => JaggedList a -> IO ()
prettyPrint JNil = return ()
prettyPrint (JCons as js) = do
    printList as
    prettyPrint js

printList :: Show a => [a] -> IO ()
printList as = putStrLn $ unwords $ map show as

-- Monad --

type MonadFarm m = (MonadWriter [String] m, MonadIO m, MonadState [Maybe String] m, MonadError FarmError m)

data FarmError = FarmlandOccupied deriving (Show, Eq)

runFarm farmMonad initialState = runExceptT $ runWriterT $ runStateT farmMonad initialState

growPlant :: MonadFarm m => String -> Int -> m ()
growPlant nov n = do
    termoFold <- get
    if n >= length termoFold then
        modify (++ (replicate (n - length termoFold) Nothing) ++ [Just nov])
    else case termoFold!!n of
        Nothing -> put $ fst $ Data.List.unzip $ map (\(x, i) -> if i == n then (Just nov, i) else (x, i)) $ zip termoFold [0..]
        Just nov' -> do
            liftIO $ putStrLn nov'
            throwError FarmlandOccupied
    tell [nov]

harvest :: MonadFarm m => m () -> m Int
harvest m = do
    (_, rem) <- listen m
    termoFold <- get
    let ujFold = doHarvest termoFold rem
    let n = harvestDiff termoFold ujFold
    put ujFold
    return n

doHarvest :: [Maybe String] -> [String] -> [Maybe String]
doHarvest termoFold rem = map (\x -> case x of {Nothing -> Nothing; Just nov -> if elem nov rem then Nothing else Just nov}) termoFold

harvestDiff :: Eq a => [a] -> [a] -> Int
harvestDiff as bs = foldr (\(a,b) acc -> if a == b then acc else acc+1) 0 $ zip as bs

-- Utils

stackTrace :: HasCallStack => String
stackTrace = concatMap
  (\(fun, s) -> "\tcall to '" ++ fun ++ "' at line " ++ show (srcLocStartLine s) ++ ", column " ++ show (srcLocStartCol s) ++ "\n") $
  getCallStack callStack

printRest :: Parser ()
printRest = get >>= traceM

evalProgram :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m ()
evalProgram = mapM_ evalStatement

runProgramT :: Monad m => [Statement] -> m (Either InterpreterError Env)
runProgramT = runExceptT . flip execStateT [] . evalProgram

runProgram :: [Statement] -> Either InterpreterError Env
runProgram = runExcept . flip execStateT [] . evalProgram

runProgramPretty :: [Statement] -> IO ()
runProgramPretty sts = do
  res <- runProgramT sts
  case res of
    Right env -> forM_ env $ \(var, val) -> putStrLn $ var ++ " == " ++ show val
    Left err -> putStrLn (message err)

parseAndRunProgram :: String -> IO ()
parseAndRunProgram s = do
  Right r <- bitraverse fail pure (parseProgram s)
  runProgramPretty r

run :: String -> Env
run s = case parseProgram s of
  Right sts -> case runProgram sts of
    Right e -> e
    _ -> error "interpreter error"
  _ -> error "parse error"

-- Parser

type Parser a = StateT String (Except String) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

asum :: MonadError e m => e -> [m a] -> m a
asum e = foldr (<|>) (throwError e)

-- Primitívek

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> throwError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= \s -> (<|> throwError ("eof: String not empty. Remaining string: "  ++ s)) (guard $ null s)

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> throwError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> throwError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> throwError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> (some (digitToInt <$> satisfy isDigit) <|> throwError "natural: number had no digits")

integer :: Parser Int
integer = maybe id (const negate) <$> optional (char '-') <*> natural

float :: Parser Double
float = do
    s <- maybe id (const negate) <$> optional (char '-')
    i <- natural
    char '.' <|> throwError "float: No digit separator"
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s (r / 10 + fromIntegral i)

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 p delim = (:) <$> (p <|> throwError "sepBy1: no elements")
                     <*> ((delim *> sepBy p delim) <|> pure [])

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy p delim = sepBy1 p delim <|> pure []

-- Whitespace-k elhagyása
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenizálás: whitespace-ek elhagyása
tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- A tokenizált parsereket '-al szoktuk jelölni

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

float' :: Parser Double
float' = tok float

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f p sep = chainr1 p (f <$ sep)

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f p sep = chainl1 p (f <$ sep)

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e] -> pure e
    [e1, e2] -> pure (f e1 e2)
    _ -> throwError "nonAssoc: too many or too few associations"

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 v op = do
  val <- v
  ( do
      opr <- op
      res <- chainr1 v op
      pure (opr val res)
    )
    <|> pure val

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
  where
    parseLeft val =
      ( do
          opr <- op
          val2 <- v
          parseLeft (opr val val2)
      )
        <|> pure val

-- Kifejezésnyelv
data Exp
  = IntLit Int                          -- 1 2 ...
  | FloatLit Double                     -- 1.0 2.11 ...
  | BoolLit Bool                        -- true false
  | SetLit [Exp]                        -- {1,2,3}
  | Var String                          -- x y ...
  | LamLit String Exp                   -- \x -> e
  | Exp :+ Exp                          -- e1 + e2
  | Exp :* Exp                          -- e1 * e2
  | Exp :- Exp                          -- e1 - e2
  | Exp :/ Exp                          -- e1 / e2
  | Exp :== Exp                         -- e1 == e2
  | Exp :$ Exp                          -- e1 $ e2
  | Not Exp                             -- not e
  | Sign Exp                            -- sign e
  | Union Exp Exp                       -- e1 ∪ e2 
  | Intersect Exp Exp                   -- e1 ∩ e2
  | Elem Exp Exp                        -- e1 ∈ e2
  | SetComprehension Exp String Exp     -- {e1 | v <- e2}
  deriving (Eq, Show, Ord)

instance Num Exp where
  (+) = (:+)
  (*) = (:*)
  abs x = x * signum x
  (-) = (:-)
  fromInteger = IntLit . fromInteger
  signum = Sign

instance Fractional Exp where
  (/) = (:/)
  fromRational = FloatLit . fromRational

{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési irány       | Kötési erősség     |
+--------------------+--------------------+--------------------+
| not, sign          | Prefix             | 20                 |
+--------------------+--------------------+--------------------+
| *                  | Jobbra             | 18                 |
+--------------------+--------------------+--------------------+
| /                  | Balra              | 16                 |
+--------------------+--------------------+--------------------+
| +                  | Jobbra             | 14                 |
+--------------------+--------------------+--------------------+
| -                  | Balra              | 12                 |
+--------------------+--------------------+--------------------+
| ∪, ∩               | Jobbra             | 11                 |
+--------------------+--------------------+--------------------+
| ==                 | Nincs              | 10                 |
+--------------------+--------------------+--------------------+
| ∈                  | Nincs              | 9                  |
+--------------------+--------------------+--------------------+
| $                  | Jobbra             | 8                  |
+--------------------+--------------------+--------------------+

-}

keywords :: [String]
keywords = ["true", "false", "not", "sign", "if", "then", "do", "for", "lam", "end", "foreach", "into"]

pNonKeyword :: Parser String
pNonKeyword = do
  res <- tok $ some (satisfy isLetter)
  res <$ (guard (res `notElem` keywords) <|> throwError "pNonKeyword: parsed a keyword")

pKeyword :: String -> Parser ()
pKeyword = string'

pAtom :: Parser Exp
pAtom = asum "pAtom: no atom matched" [
  FloatLit <$> float',
  IntLit <$> integer',
  BoolLit True <$ pKeyword "true",
  BoolLit False <$ pKeyword "false",
  SetLit <$> between (char' '{') (sepBy pExp $ char' ',') (char' '}'),
  LamLit <$> (pKeyword "lam" *> pNonKeyword) <*> (string' "->" *> pExp),
  Var <$> pNonKeyword,
  (between (char' '{') (SetComprehension <$> pExp <*> (char' '|' *> pNonKeyword) <*> (string' "<-" *> pExp)) (char' '}')),
  between (char' '(') pExp (char' ')')
             ]

pNot :: Parser Exp
pNot = (Not <$> (pKeyword "not" *> pNot)) <|> (Sign <$> (pKeyword "sign" *> pNot)) <|> pAtom

pMul :: Parser Exp
pMul = chainr1 pNot ((:*) <$ char' '*')

pDiv :: Parser Exp
pDiv = chainl1 pMul ((:/) <$ char' '/')

pAdd :: Parser Exp
pAdd = chainr1 pDiv ((:+) <$ char' '+')

pMinus :: Parser Exp
pMinus = chainl1 pAdd ((:-) <$ char' '-')

pUnionIntersect :: Parser Exp
pUnionIntersect = chainr1 pMinus ((Union) <$ char' '∪' <|> (Intersect) <$ char' '∩')

pEq :: Parser Exp
pEq = nonAssoc (:==) pUnionIntersect (string' "==")

pElem :: Parser Exp
pElem = nonAssoc (Elem) pEq (char' '∈')

pDollar :: Parser Exp
pDollar = chainr1 pElem ((:$) <$ char' '$')

pExp :: Parser Exp -- táblázat legalja
pExp = pDollar

-- Állítások: értékadás, elágazások, ciklusok
data Statement
  = If Exp [Statement]                -- if e then p end
  | While Exp [Statement]             -- while e do p end
  | Assign String Exp                 -- v := e
  | ForEach Exp String [Statement]    -- foreach e into v do p end
  deriving (Eq, Show)

program :: Parser [Statement]
program = tok $ many (statement <* char' ';')

statement :: Parser Statement
statement = asum "statement: no statement matched" [sIf, sWhile, sAssign, sForEach]

sIf :: Parser Statement
sIf = If <$> (pKeyword "if" *> pExp) <*> (pKeyword "then" *> program <* pKeyword "end")

sWhile :: Parser Statement
sWhile = While <$> (pKeyword "while" *> pExp) <*> (pKeyword "do" *> program <* pKeyword "end")

sAssign :: Parser Statement
sAssign = Assign <$> pNonKeyword <*> (pKeyword ":=" *> pExp)

sForEach :: Parser Statement
sForEach = ForEach <$> (pKeyword "foreach" *> pExp) <*> (pKeyword "into" *> pNonKeyword) <*> (pKeyword "do" *> program <* pKeyword "end")

parseProgram :: String -> Either String [Statement]
parseProgram s = case runParser (topLevel program) s of
  Left e -> Left e
  Right (x,_) -> Right x

-- Interpreter
-- Kiértékelt értékek típusa:
data Val
  = VInt Int              -- int kiértékelt alakban
  | VFloat Double         -- double kiértékelt alakban
  | VBool Bool            -- bool kiértékelt alakban
  | VLam String Env Exp   -- lam kiértékelt alakban
  | VSet [Val]            -- halmaz kiértékelt alakban
  deriving (Eq, Show, Ord)

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError { message :: String } -- típushiba üzenettel
  | ScopeError { message :: String } -- variable not in scope üzenettel
  | DivByZeroError { message :: String } -- 0-val való osztás hibaüzenettel
  deriving (Eq, Show)

-- Értékeljünk ki egy kifejezést!
evalExps :: (HasCallStack, MonadError InterpreterError m) => [Exp] -> Env -> m [Val]
evalExps [] env = return []
evalExps (e:es) env = do
    v <- evalExp e env
    vs <- evalExps es env
    return (v:vs)

evalExp :: (HasCallStack, MonadError InterpreterError m) => Exp -> Env -> m Val
evalExp exp env = case exp of
  IntLit i -> return (VInt i)
  FloatLit f -> return (VFloat f)
  BoolLit b -> return (VBool b)
  LamLit s e -> return (VLam s env e)
  SetLit es -> do
    vs <- evalExps es env
    return $ VSet (sort $ nub vs)
  Not e -> evalExp e env >>= \case
    VBool b -> return (VBool $ not b)
    _       -> throwError (TypeError $ "Type error in the operand of not\nSTACK TRACE:\n" ++ stackTrace)
  Sign e -> evalExp e env >>= \case
    VInt i -> return (VInt $ signum i)
    VFloat f -> return (VFloat $ signum f)
    _       -> throwError (TypeError $ "Type error in the operand of sign\nSTACK TRACE:\n" ++ stackTrace)
  Var str -> case lookup str env of
    Just v -> return v
    Nothing -> throwError (ScopeError $ "Variable not in scope: " ++ str ++ "\nSTACK TRACE:\n" ++ stackTrace)
  e1 :+ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VInt (i1 + i2))
      (VFloat f1, VFloat f2) -> return (VFloat (f1 + f2))
      _ -> throwError (TypeError $ "Type error in the operands of +\nSTACK TRACE:\n" ++ stackTrace)
  e1 :- e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VInt (i1 - i2))
      (VFloat f1, VFloat f2) -> return (VFloat (f1 - f2))
      _ -> throwError (TypeError $ "Type error in the operands of -\nSTACK TRACE:\n" ++ stackTrace)
  e1 :* e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VInt (i1 * i2))
      (VFloat f1, VFloat f2) -> return (VFloat (f1 * f2))
      _ -> throwError (TypeError $ "Type error in the operands of *\nSTACK TRACE:\n" ++ stackTrace)
  e1 :/ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt 0) -> throwError (DivByZeroError $ "Cannot divide by integer zero\nSTACK TRACE:\n" ++ stackTrace)
      (VInt i1, VInt i2) -> return (VInt (div i1 i2))
      (VFloat f1, VFloat f2) | abs f2 < 0.0001 -> throwError (DivByZeroError $ "Cannot divide by float zero\nSTACK TRACE:\n" ++ stackTrace)
      (VFloat f1, VFloat f2) -> return (VFloat (f1 / f2))
      _ -> throwError (TypeError $ "Type error in the operands of /\nSTACK TRACE:\n" ++ stackTrace)
  e1 :== e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VBool (i1 == i2))
      (VFloat f1, VFloat f2) -> return (VBool (f1 == f2))
      (VBool b1, VBool b2) -> return (VBool (b1 == b2))
      _ -> throwError (TypeError $ "Type error in the operands of ==\nSTACK TRACE:\n" ++ stackTrace)
  e1 :$ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case v1 of
      (VLam s env' e) -> evalExp e ((s, v2) : env')
      _ -> throwError (TypeError $ "Type error in the operands of function application\nSTACK TRACE:\n" ++ stackTrace)
  Elem e1 e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case v2 of
      (VSet s) -> if elem v1 s then return $ VBool True else return $ VBool False
      _ -> throwError (TypeError $ "Type error in the operands of ∈\nSTACK TRACE:\n" ++ stackTrace)
  Union e1 e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VSet s1, VSet s2) -> return (VSet (union s1 s2))
      _ -> throwError (TypeError $ "Type error in the operands of ∪\nSTACK TRACE:\n" ++ stackTrace)
  Intersect e1 e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VSet s1, VSet s2) -> return (VSet (intersect s1 s2))
      _ -> throwError (TypeError $ "Type error in the operands of ∩\nSTACK TRACE:\n" ++ stackTrace)
  SetComprehension e1 str e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VLam s env' e, VSet set) -> undefined
      _ -> throwError (TypeError $ "Type error in the operands of set comprehension\nSTACK TRACE:\n" ++ stackTrace)
      
updateEnv :: Env -> String -> Val -> Env
updateEnv [] s v = [(s,v)]
updateEnv ((s', v'):xs) s v
  | s == s' = (s, v) : xs
  | otherwise = (s', v') : updateEnv xs s v

inBlockScope :: MonadState Env m => m a -> m a
inBlockScope f = do
  env <- get
  a <- f
  modify (take (length env))
  pure a

-- Állítás kiértékelésénér egy state-be eltároljuk a jelenlegi környezetet
evalStatement :: (HasCallStack, MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
evalStatement st = case st of
  Assign x e -> do
    env <- get
    v <- evalExp e env
    put (updateEnv env x v)
  If e sts -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool True -> inBlockScope $ evalProgram sts
      VBool _ -> pure ()
      _ -> throwError (TypeError $ "Type error in the condition of 'if'\nSTACK TRACE:\n" ++ stackTrace)
  While e sts -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool True -> do
        inBlockScope $ evalProgram sts
        evalStatement (While e sts)
      VBool _ -> pure ()
      _ -> throwError (TypeError $ "Type error in the condition of 'while'\nSTACK TRACE:\n" ++ stackTrace)
  _ -> undefined

--- PÉLDA PROGRAMOK

p1 :: String
p1 = concat [
  "x := 1;",
  "if (x == 1) then " ++ concat [
      "y := 2;",
      "x := 10 - y;"
   ] ++ "end"
            ]
p2 :: String
p2 = concat [
  "func := lam x -> x + 1;",
  "while (not ((func $ 0) == 1024)) do " ++ concat [
      "func := lam x -> (func $ x) * 2;"
  ] ++ "end;",
  "x := func $ 10"
            ]
