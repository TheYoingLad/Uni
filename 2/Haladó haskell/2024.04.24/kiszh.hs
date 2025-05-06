{-# LANGUAGE LambdaCase #-}
module Kiszh where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.List
import Data.Bifunctor

-- Parser hibaüzenettel
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

-- Ennek segítségével tudunk már természetes számokat parseolni
natural :: Parser Int
natural = foldl (\acc x -> acc * 10 + x) 0 <$> some digit

-- Parseoljunk be egy egész számot! (Előjel opcionális)
integer :: Parser Int
integer = do
  sign <- optional (negate <$ char '-' <|> id <$ char '+')
  v <- natural
  case sign of
    Nothing -> pure v
    Just f -> pure (f v)

-- Bónusz: Float parser (nem kell tudni, csak érdekes)
float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-')
    i <- natural
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s * (r / 10 + fromIntegral i)

between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 a delim = (:) <$> a <*> many (delim *> a)

-- Ugyanaz mint a fenti, de nem követeli meg, hogy legalább 1 legyen
sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy a delim = sepBy1 a delim <|> pure []

-- Whitespace-k elhagyása
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenizálás: whitespace-ek elhagyása
tok :: Parser a -> Parser a
tok p = p <* ws -- Itt a <* kell mert a bal parser eredménye érdekes

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- A tokenizált parsereket '-al szoktuk jelölni

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

-- Hajtogató parserek

-- Az alábbi parserek kifejezésnyelvekhez lesznek a segédparsereink

-- Jobbra asszocialó kifejezést parseoljon.
-- Sep által elválasztott kifejezéseket gyűjtsön össze, majd azokra a megfelelő sorrendbe alkalmazza a függvényt
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f a sep = foldr1 f <$> sepBy1 a sep

-- Ugyanaz mint a rightAssoc csak balra
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f a sep = foldl1 f <$> sepBy1 a sep

-- Kompetensebb verziói a fenti parsereknek

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
  = IntLit Int -- integer literál pl 1, 2
  | FloatLit Double -- lebegőpontos szám literál, pl 1.0 vagy 2.3
  | Var String -- változónév
  | Exp :+ Exp -- összeadás
  | Exp :* Exp -- szorzás
  | Exp :^ Exp -- hatványozás
  | Exp :- Exp
  | Exp :/ Exp
  | Exp :% Exp
  | Exp :@ Exp
  deriving (Eq, Show)

{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési Irány       | Kötési Erősség     |
+--------------------+--------------------+--------------------+
| ^                  | Jobbra             | 16                 |
+--------------------+--------------------+--------------------+
| *                  | Balra              | 14                 |
+--------------------+--------------------+--------------------+
| /                  | Jobbra             | 13                 |
+--------------------+--------------------+--------------------+
| +, -               | Balra              | 12                 |
+--------------------+--------------------+--------------------+
-}

pAtom :: Parser Exp
pAtom = (FloatLit <$> (float <* ws)) <|> (IntLit <$> integer') <|> (Var <$> some (satisfy isLetter) <* ws)
  <|> (between (char' '(') pAdd (char' ')'))

pPow :: Parser Exp --16
pPow = chainr1 pAtom ((:^) <$ char' '^')

pPerc :: Parser Exp --15
pPerc = chainr1 pPow ((:%) <$ char' '%')

pMulAt :: Parser Exp --14
pMulAt = chainl1 pPerc ((:*) <$ char' '*' <|> (:@) <$ char' '@')

pDiv :: Parser Exp --13
pDiv = chainr1 pMulAt ((:/) <$ char' '/')

pAdd :: Parser Exp --12
pAdd = chainl1 pDiv ((:+) <$ char' '+' <|> (:-) <$ char' '-')