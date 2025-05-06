{-# OPTIONS_GHC -Wno-unused-imports #-}
module Kiszh where

import Data.List
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class


-- Modellezzünk egy hálózati routert az alábbi környezetekkel
-- Állapotváltozási környezet: jelenlegi routing tábla [(String, Int)] típusú ahol az első paraméter egy string alapú ip cím a második egy port szám, pl [("192.168.0.2", 10)]
-- Írási környezet: [(Int, String)] típusú környezet ami az adott portokra kiküldött üzenetet jelzi, pl ha az 1. és 3. portra ki akarnánk küldeni, hogy alma, akkor [(1, "alma"), (3, "alma")]

runRouter :: StateT [(String, Int)] (Writer [(Int, String)]) a -> ([(String, Int)], [(Int, String)], a)
runRouter stwt = (\((a,b),c) -> (b,c,a)) (runWriter (runStateT stwt []))

type MonadRouter m = (MonadState [(String, Int)] m, MonadWriter [(Int, String)] m)
-- (1 PONT)
-- Definiáljuk az acknowledge függvényt, amely egy IP címet és egy portot vár paraméterül.
-- Ha a port már foglalt az állapotban, küldjünk arra a portra egy "NOPE" üzenetet.
-- Ha a port nem foglalt, akkor írjuk bele a routing táblába az IP címet és a portot és küldjünk arra a portra egy "YEP" üzenetet.
-- A függvény eredménye monádba csomagolt () legyen
-- Tip: használd a lookup függvényt

acknowledge' :: MonadRouter m => String -> Int -> m ()
acknowledge' ip port = do
    table <- get
    case (lookup port $ map (\(a,b) -> (b,a)) table) of 
        (Just _) -> do
            tell [(port, "NOPE")]
        Nothing -> do
            modify((ip, port):)
            tell [(port, "YEP")]
    return ()
    
-- (1 PONT)
-- Definiáljuk a broadcast üzenetet ami minden portra kiküldi a saját IP címét az alábbi formátumban:
-- "THIS YOU <IP CÍM IDE>?"
-- Az eredmény a kiküldött üzenetek száma legyen

broadcast :: MonadRouter m => m Int
broadcast = do
    table <- get
    case table of
        [] -> return 0
        ((ip, port):xs) -> do
            tell [(port, "THIS YOU " ++ (show ip) ++ "?")]
            put xs
            m <- broadcast
            return (m + 1)