{-# LANGUAGE LambdaCase #-}
module Gy09 where

import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable

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
eof = get >>= (<|> throwError "eof: String not empty") . guard . null

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
  deriving (Eq, Show)

{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési irány       | Kötési erősség     |
+--------------------+--------------------+--------------------+
| not                | Prefix             | 20                 |
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
keywords = ["true", "false", "lam", "not", "while", "do", "end", "if", "then", "do", "for", "funk", "begin", "delete", "create", "jump"]

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

pMul :: Parser Exp
pMul = chainr1 pNot ((:*) <$ char' '*')

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

-- Állítások: értékadás, elágazások, ciklusok
data Statement
  = If Exp [Statement]                            -- if e then sts end;
  | IfElse Exp [Statement] [Statement]            -- if e then sts else sts end;
  | While Exp [Statement]                         -- while e do sts end;
  | DoWhile [Statement] Exp                       -- do sts while e end;
  | For Statement Exp Statement [Statement]       -- for st; e; st; do sts end;
  | Assign String Exp                             -- v := e
  | Funk String [String] [Statement]              -- funk var (vars) begin sts end;
  | Delete String                                 -- delete var
  | Create String                                 -- create var
  | Label String                                  -- @var
  | Jump String                                   -- jump var
  deriving Show
-- Írjunk ezekre parsereket!
-- Egy programkód egyes sorait ;-vel választjuk el

program :: Parser [Statement]
program = many statement

statement :: Parser Statement
statement = asum [sIf, sWhile, sAssign, sDoWhile, sFor, sIfElse, sFunk, sDelete, sCreate, sLabel, sJump] <* char' ';'

sJump :: Parser Statement
sJump = do
  pKeyword "jump"
  v <- pNonKeyword
  return (Jump v)

sLabel :: Parser Statement
sLabel = do
  char' '@'
  v <- pNonKeyword
  return (Label v)

sCreate :: Parser Statement
sCreate = do
  pKeyword "delete"
  v <- pNonKeyword
  return (Delete v)

sDelete :: Parser Statement
sDelete = do
  pKeyword "delete"
  v <- pNonKeyword
  return (Delete v)

sFunk :: Parser Statement
sFunk = do
  pKeyword "funk"
  v <- pNonKeyword
  vars <- between (char' '(') (many pNonKeyword) (char' ')')
  pKeyword "begin"
  prog <- program
  pKeyword "end"
  return (Funk v vars prog)

sIfElse :: Parser Statement
sIfElse = do
  pKeyword "if"
  e <- pExp
  pKeyword "then"
  prog1 <- program
  pKeyword "else"
  prog2 <- program
  pKeyword "end"
  return (IfElse e prog1 prog2)

sFor :: Parser Statement
sFor = do
  pKeyword "for"
  s1 <- statement
  e <- pExp <* char' ';'
  s2 <- statement
  pKeyword "do"
  prog <- program
  pKeyword "end"
  return (For s1 e s2 prog)

sDoWhile :: Parser Statement
sDoWhile = do
  pKeyword "do"
  prog <- program
  pKeyword "while"
  e <- pExp
  pKeyword "end"
  return (DoWhile prog e)

sIf :: Parser Statement
sIf = do
  pKeyword "if"
  e <- pExp
  pKeyword "then"
  prog <- program
  pKeyword "end"
  return (If e prog)

sWhile :: Parser Statement
sWhile = do
  pKeyword "while"
  e <- pExp
  pKeyword "do"
  prog <- program
  pKeyword "end"
  return (While e prog)

sAssign :: Parser Statement
sAssign = do
  var <- pNonKeyword
  string' ":="
  e <- pExp
  return (Assign var e)

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

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError String -- típushiba üzenettel
  | ScopeError String -- variable not in scope üzenettel
  | DivByZeroError String -- 0-val való osztás hibaüzenettel

-- Az interpreter típusát nem adjuk meg explicit, hanem használjuk a monád transzformerek megkötéseit!
-- Értékeljünk ki egy kifejezést!
evalExp :: MonadError InterpreterError m => Exp -> Env -> m Val
evalExp exp env = case exp of
  IntLit i -> return $ VInt i
  FloatLit d -> return $ VFloat d
  BoolLit b -> return $ VBool b
  Var s -> case lookup s env of
    Just v -> return v
    _ -> throwError(ScopeError $ "Variable not in scope: " ++ s)
  e1 :+ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VInt $ i1 + i2
      (VFloat d1, VFloat d2) -> return $ VFloat $ d1 + d2
      _ -> throwError(TypeError $ "type error in the operands of +")
  e1 :* e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VInt $ i1 * i2
      (VFloat d1, VFloat d2) -> return $ VFloat $ d1 * d2
      _ -> throwError(TypeError $ "type error in the operands of *")
  e1 :- e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VInt $ i1 - i2
      (VFloat d1, VFloat d2) -> return $ VFloat $ d1 - d2
      _ -> throwError(TypeError $ "type error in the operands of -")
  e1 :/ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> if i2==0 then throwError(DivByZeroError $ "cannot divide by 0") else return $ VInt $ (div i1 i2)
      (VFloat d1, VFloat d2) -> if d2==0 then throwError(DivByZeroError $ "cannot divide by 0") else return $ VFloat $ d1 / d2
      _ -> throwError(TypeError $ "type error in the operands of /")
  e1 :== e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VBool $ i1 == i2
      (VFloat d1, VFloat d2) -> return $ VBool $ d1 == d2
      (VBool b1, VBool b2) -> return $ VBool $ b1 == b2
      _ -> throwError(TypeError $ "type error in the operands of ==")
  Not e -> do
    v <- evalExp e env
    case v of
      (VBool b) -> return $ VBool $ not b
      _ -> throwError(TypeError $ "operand of not is not a bool")
  LamLit s e -> return $ VLam s env e
  e1 :$ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case v1 of
      (VLam s env' e) -> evalExp e ((s, v2):env')
      _ -> throwError(TypeError $ "type error in the operands of $")

-- Állítás kiértékelésénér egy state-be eltároljuk a jelenlegi környezetet
evalStatement :: (MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
evalStatement st = case st of
  (If exp sts) -> do
    env <- get
    let l = genericLength env
    val <- evalExp exp env
    case val of
      VBool True -> do
        evalProgram sts
        modify $ take l
      VBool False -> return ()
      _ -> throwError(TypeError $ "operand of if is not a bool")
  (While exp sts) -> do
    env <- get
    let l = genericLength env
    val <- evalExp exp env
    case val of
      VBool True -> do
        evalProgram sts
        modify $ take l
        evalStatement $ While exp sts
      VBool False -> return ()
      _ -> throwError(TypeError $ "operand of while is not a bool")
  (Assign s exp) -> do
    env <- get
    val <- evalExp exp env
    put $ updateEnv env s val
  (DoWhile sts exp) -> do
    env <- get
    let l = genericLength env
    evalProgram sts
    modify $ take l
    val <- evalExp exp env
    case val of
      VBool True -> evalStatement $ DoWhile sts exp
      VBool False -> return ()
      _ -> throwError(TypeError $ "operand of do while is not a bool")
  (For init exp incr sts) -> do
    env <- get
    let l = genericLength env
    initVal <- evalExp exp env
    modify $ ((_, initVal):)
    _


updateEnv :: Env -> String -> Val -> Env
updateEnv [] s val = [(s, val)]
updateEnv ((s, val):env) s' val' = if s == s' then (s, val'):env else (s,val):updateEnv env s' val'

evalProgram :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m ()
evalProgram = mapM_ evalStatement

-- Egészítsük ki a nyelvet egy print állítással (hint: MonadIO megkötés)
-- Egészítsük ki a nyelvet más típusokkal (tuple, either stb)
