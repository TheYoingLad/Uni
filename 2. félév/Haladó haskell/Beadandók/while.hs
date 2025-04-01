{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module While where

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
import Data.Foldable
import GHC.Stack
import Debug.Trace
import Data.Either

-- Utils

stackTrace :: HasCallStack => String
stackTrace = concatMap
  (\(fun, s) -> "\tcall to '" ++ fun ++ "' at line " ++ show (srcLocStartLine s) ++ ", column " ++ show (srcLocStartCol s) ++ "\n") $
  getCallStack callStack

printRest :: Parser ()
printRest = get >>= traceM

evalProgram :: (MonadError InterpreterError m, MonadState Env m) => Program -> m ()
evalProgram = mapM_ evalStatement

runProgramT :: Monad m => Program -> m (Either InterpreterError Env)
runProgramT = runExceptT . flip execStateT [] . evalProgram

runProgram :: Program -> Either InterpreterError Env
runProgram = runExcept . flip execStateT [] . evalProgram

runProgramPretty :: Program -> IO ()
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

data Exp
  = IntLit Int           -- 1 2 ...
  | FloatLit Double      -- 1.0 2.11 ...
  | BoolLit Bool         -- true false
  | Var String           -- x y ...
  | LamLit String Exp    -- \x -> e
  | Exp :+ Exp           -- e1 + e2
  | Exp :* Exp           -- e1 * e2
  | Exp :- Exp           -- e1 - e2
  | Exp :/ Exp           -- e1 / e2
  | Exp :== Exp          -- e1 == e2
  | Exp :$ Exp           -- e1 $ e2
  | Not Exp              -- not e
  | And Exp Exp          -- and e1 e2
  | Or Exp Exp           -- or e1 e2
  deriving (Eq, Show)

{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési irány       | Kötési erősség     |
+--------------------+--------------------+--------------------+
| not                | Prefix             | 21                |
+--------------------+--------------------+--------------------+
| and                | Prefix             | 20                 |
+--------------------+--------------------+--------------------+
| or                 | Prefix             | 19                 |
+--------------------+--------------------+--------------------+
| *                  | Jobbra             | 18                 |
+--------------------+--------------------+--------------------+
| /                  | Balra              | 16                 |
+--------------------+--------------------+--------------------+
| +                  | Jobbra             | 14                 |
+--------------------+--------------------+--------------------+
| -                  | Balra              | 12                 |
+--------------------+--------------------+--------------------+
| ==                 | Nincs              | 10                 |
+--------------------+--------------------+--------------------+
| $                  | Jobbra             | 8                  |
+--------------------+--------------------+--------------------+
-}

keywords :: [String]
keywords = ["true", "false", "not", "and", "or", "if", "then", "else", "end", "while", "do", "lam"]

pNonKeyword :: Parser String
pNonKeyword = do
  res <- tok $ some (satisfy isLetter)
  res <$ (guard (res `notElem` keywords) <|> throwError "pNonKeyword: parsed a keyword")

pKeyword :: String -> Parser ()
pKeyword = string'

pAtom :: Parser Exp
pAtom = asum [
  FloatLit <$> float',
  IntLit <$> integer',
  BoolLit True <$ pKeyword "true",
  BoolLit False <$ pKeyword "false",
  LamLit <$> (pKeyword "lam" *> pNonKeyword) <*> (string' "->" *> pExp),
  Var <$> pNonKeyword,
  between (char' '(') pExp (char' ')')
             ] <|> throwError "pAtom: no literal, var or bracketed matches"

pNot :: Parser Exp
pNot = (Not <$> (pKeyword "not" *> pNot)) <|> pAtom

pAnd :: Parser Exp
pAnd = (And <$> (pKeyword "and" *> pNot) <*> pNot) <|> pNot

pOr :: Parser Exp
pOr = (Or <$> (pKeyword "or" *> pAnd) <*> pAnd) <|> pAnd

pMul :: Parser Exp
pMul = chainr1 pOr ((:*) <$ char' '*')

pDiv :: Parser Exp
pDiv = chainl1 pMul ((:/) <$ char' '/')

pAdd :: Parser Exp
pAdd = chainr1 pDiv ((:+) <$ char' '+')

pMinus :: Parser Exp
pMinus = chainl1 pAdd ((:-) <$ char' '-')

pEq :: Parser Exp
pEq = nonAssoc (:==) pMinus (string' "==")

pDollar :: Parser Exp
pDollar = chainr1 pEq ((:$) <$ char' '$')

pExp :: Parser Exp -- táblázat legalja
pExp = pDollar

data Statement
  = If Exp [Statement] [Statement]    -- if e then p1 else p2 end    ezt át kellett írnom, hogy megfeleljen a teszteknek és a feladat leírásának
  | While Exp [Statement]             -- while e do p end
  | Assign String Exp                 -- v := e
  deriving (Eq, Show)

type Program = [Statement]

-- Állítások: értékadás, elágazások, ciklusok

program :: Parser Program
program = tok $ many (statement <* char' ';')

statement :: Parser Statement
statement = asum [sIf, sWhile, sAssign]

sIf :: Parser Statement
sIf = If <$> (pKeyword "if" *> pExp) <*> (pKeyword "then" *> program <* pKeyword "else") <*> (program <* pKeyword "end")

sWhile :: Parser Statement
sWhile = While <$> (pKeyword "while" *> pExp) <*> (pKeyword "do" *> program <* pKeyword "end")

sAssign :: Parser Statement
sAssign = Assign <$> pNonKeyword <*> (pKeyword ":=" *> pExp)

parseProgram :: String -> Either String Program
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
  deriving Show

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError { message :: String } -- típushiba üzenettel
  | ScopeError { message :: String } -- variable not in scope üzenettel
  | DivByZeroError { message :: String } -- 0-val való osztás hibaüzenettel
  deriving Show

-- Értékeljünk ki egy kifejezést!
evalExp :: (HasCallStack, MonadError InterpreterError m) => Exp -> Env -> m Val
evalExp exp env = case exp of
  IntLit i -> return (VInt i)
  FloatLit f -> return (VFloat f)
  BoolLit b -> return (VBool b)
  LamLit s e -> return (VLam s env e)
  Not e -> evalExp e env >>= \case
    VBool b -> return (VBool $ not b)
    _ -> throwError (TypeError $ "Type error in the operand of not\nSTACK TRACE:\n" ++ stackTrace)
  And e1 e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VBool b1, VBool b2) -> return (VBool (b1 && b2))
      _ -> throwError (TypeError $ "Type error in the operands of ==\nSTACK TRACE:\n" ++ stackTrace)
  Or e1  e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VBool b1, VBool b2) -> return (VBool (b1 || b2))
      _ -> throwError (TypeError $ "Type error in the operands of ==\nSTACK TRACE:\n" ++ stackTrace)
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

updateEnv :: Env -> String -> Val -> Env
updateEnv [] s v = [(s,v)]
updateEnv ((s', v'):xs) s v
  | s == s' = (s, v) : xs
  | otherwise = (s', v') : updateEnv xs s v

deleteFromEnv :: Env -> String -> Env
deleteFromEnv [] _ = []
deleteFromEnv ((s, v):env) s'
  | s == s' = env
  | otherwise = (s, v) : deleteFromEnv env s'

inBlockScope :: MonadState Env m => m a -> m a
inBlockScope f = do
  env <- get
  a <- f
  modify (take (length env))
  pure a

-- Állítás kiértékelésénél egy state-be eltároljuk a jelenlegi környezetet
evalStatement :: (HasCallStack, MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
evalStatement st = case st of
  Assign x e -> do
    env <- get
    v <- evalExp e env
    put (updateEnv env x v)
  If e sts1 sts2 -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool True -> inBlockScope $ evalProgram sts1
      VBool _ -> inBlockScope $ evalProgram sts2
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

-----------------------------FELADAT------------------------------

check :: Program -> Bool
check p = case runCheckProgram p of
  Left _ -> False
  _ -> True

runCheckProgram :: Program -> Either InterpreterError Env
runCheckProgram = runExcept . flip execStateT [] . checkProgram

checkProgram :: (MonadError InterpreterError m, MonadState Env m) => Program -> m ()
checkProgram = mapM_ checkStatement

checkStatement :: (HasCallStack, MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
checkStatement st = case st of
  Assign x e -> do
    env <- get
    v' <- evalExp e env
    case lookup x env of
      Just v -> case (v, v') of
        (VInt _, VInt _) -> put (updateEnv env x v')
        (VBool _, VBool _) -> put (updateEnv env x v')
        (VFloat _, VFloat _) -> put (updateEnv env x v')
        _ -> throwError (TypeError $ "Type error in the operand of 'assign'\nSTACK TRACE:\n" ++ stackTrace)
      _ -> put (updateEnv env x v')
  If e sts1 sts2 -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool True -> inBlockScope $ checkProgram sts1
      VBool _ -> inBlockScope $ checkProgram sts2
      _ -> throwError (TypeError $ "Type error in the condition of 'if'\nSTACK TRACE:\n" ++ stackTrace)
  While e sts -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool _ -> inBlockScope $ checkProgram sts
      _ -> throwError (TypeError $ "Type error in the condition of 'while'\nSTACK TRACE:\n" ++ stackTrace)

-----------------------------TESZTELÉS------------------------------

-- rövidítések
i0 :: Exp
i0    = IntLit 10
true :: Exp
true  = BoolLit True
false :: Exp
false = BoolLit False
fi :: Exp
fi    = LamLit "x" (Var "x")
vx :: Exp
vx    = Var "x"
vy :: Exp
vy    = Var "x"
vz :: Exp
vz    = Var "z"

amap :: Statement -> Statement
amap = \x -> case x of { (Assign a b) -> Assign a (fi :$ b); _ -> x }

-- esetek, amire check True-t ad
suc1 :: Program
suc1 = [
    Assign "x" i0
  , Assign "x" i0
  ]
suc2 :: Program
suc2 = [
    Assign "x" i0
  , Assign "y" vx
  , Assign "y" (vx :+ vx)
  ]
suc3 :: Program
suc3 = [
    While true [Assign "x" i0]
  , Assign "x" true
  ]
suc4 :: Program
suc4 = [
    Assign "x" true
  , If vx [Assign "y" i0] [Assign "y" i0]
  , Assign "y" true
  ]
suc5 :: Program
suc5  = [Assign "x" (i0 :== i0)]
suc6 :: Program
suc6  = [Assign "x" (true :== true)]
suc7 :: Program
suc7  = [Assign "x" (Or (i0 :== i0) (i0 :== i0))]
suc8 :: Program
suc8  = [Assign "x" (And (i0 :== i0) (Or true false))]
suc9 :: Program
suc9  = [Assign "x" (Not true)]
suc10 :: Program
suc10 = [Assign "x" (Not (true :== false))]
suc11 :: Program
suc11 = [Assign "x" (i0 :* i0)]
suc12 :: Program
suc12 = [Assign "x" ((i0 :* i0):- i0)]
suc13 :: Program
suc13 = [Assign "x" ((i0 :* i0) :+ i0)]
suc14 :: Program
suc14 = map amap suc5
suc15 :: Program
suc15 = map amap suc6
suc16 :: Program
suc16 = map amap suc7
suc17 :: Program
suc17 = map amap suc8
suc18 :: Program
suc18 = map amap suc9
suc19 :: Program
suc19 = map amap suc10
suc20 :: Program
suc20 = map amap suc11
suc21 :: Program
suc21 = map amap suc12
suc22 :: Program
suc22 = map amap suc13
suc23 :: Program
suc23 = map amap suc14

-- esetek, amire check False-t ad
fail1 :: Program
fail1 = [
    Assign "x" i0
  , Assign "x" false
  ]
fail2 :: Program
fail2 = [ Assign "x" (i0 :+ false) ]
fail3 :: Program
fail3 = [
    Assign "x" true
  , While true [Assign "x" i0]
  ]
fail4 :: Program
fail4 = [
    Assign "x" true
  , Assign "y" true
  , If (Var "x") [Assign "y" i0] [Assign "y" i0]
  ]
fail5 :: Program
fail5  = [Assign "x" (i0 :== false)]
fail6 :: Program
fail6  = [Assign "x" vx]
fail7 :: Program
fail7  = [While vx []]
fail8 :: Program
fail8  = [Assign "x" (Or i0 (i0 :== i0))]
fail9 :: Program
fail9  = [Assign "x" (And (i0 :== i0) i0)]
fail10 :: Program
fail10 = [Assign "x" (Not i0)]
fail11 :: Program
fail11 = [Assign "x" (i0 :* true)]
fail12 :: Program
fail12 = [Assign "x" ((i0 :* i0) :- false)]
fail13 :: Program
fail13 = [Assign "x" ((i0 :* i0) :+ (i0 :== i0))]
fail14 :: Program
fail14 = map amap fail5
fail15 :: Program
fail15 = map amap fail6
fail16 :: Program
fail16 = map amap fail7
fail17 :: Program
fail17 = map amap fail8
fail18 :: Program
fail18 = map amap fail9
fail19 :: Program
fail19 = map amap fail10
fail20 :: Program
fail20 = map amap fail11
fail21 :: Program
fail21 = map amap fail12
fail22 :: Program
fail22 = map amap fail13
fail23 :: Program
fail23 = map amap fail14

-- a test függvénnyel lehet tömören kinyomtatni a teszteseteket

successes :: [Program]
successes =
  [suc1, suc2, suc3, suc4, suc5, suc6, suc7, suc8, suc9, suc10, suc11, suc12, suc13, suc14, suc15, suc16, suc17, suc18, suc19, suc20, suc21, suc22, suc23]

failures :: [Program]
failures =
  [ fail1, fail2, fail3, fail4, fail5, fail6, fail7, fail8, fail9, fail10, fail11, fail12, fail13, fail14, fail15, fail16, fail17, fail18, fail19, fail20, fail21, fail22, fail23 ]

test :: IO ()
test = do
  forM_ (zip [1..] successes) $ \(i, p) -> do
    putStrLn $ "success " ++ show i ++ ": " ++ show (check p)

  forM_ (zip [1..] failures) $ \(i, p) -> do
    putStrLn $ "failure " ++ show i ++ ": " ++ show (check p)
