{-# LANGUAGE DeriveFunctor #-}

module Kiszh where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Foldable
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  (Parser g) >>= f = Parser $ \s -> g s >>= \(a, s') -> runParser (f a) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser f) <|> (Parser g) = Parser $ \s -> f s <|> g s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (c:cs) | p c -> Just (c, cs)
  _            -> Nothing

eof :: Parser ()
eof =  Parser $ \s -> ((), []) <$ guard (null s)

char :: Char -> Parser ()
char c = void $ satisfy (== c)

anychar :: Parser Char
anychar = satisfy (const True)

string :: String -> Parser ()
string = mapM_ char


-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    c        - Parseol egy c karaktert                char 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anyChar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
-}

parseDigit :: Parser Int
parseDigit = digitToInt <$> satisfy isDigit

p1 :: Parser ()
p1 = char 'c' <|> char 'C' >> string "razy" >> char 't' <|> char 'T' >> string "ype" >> optional (satisfy (\x -> (digitToInt x) >= 1 && (digitToInt x) <= 3)) >> eof

p2 :: Parser ()
p2 = many (char 'c') >> some (char 'i') >> replicateM 3 (char 'c') >> optional((parseDigit >> pure ()) <|> char 'a') >> many anychar >> pure ()