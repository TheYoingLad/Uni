module Kiszh where
 
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

accessAt :: Int -> Reader [a] (Maybe a)
accessAt 0 = do
    xs <- ask
    case xs of
        [] -> return Nothing
        (x:xs) -> return (Just x)
accessAt i = do
    xs <- ask
    case xs of
        [] -> return Nothing
        (x:xs) -> do
            local (\_ -> xs) (accessAt (i-1))

performOperation :: (Show a, Eq a) => (a -> b) -> [a] -> a -> Except String b        
performOperation f as a = do
    when (elem a as) $ throwError (show a ++ " benne volt a tiltott elemek listájában, a számítást nem lehet elvégezni")
    return (f a)

performOperations :: (Eq a, Show a) => (a -> b) -> [a] -> [a] -> Except String [b]
performOperations f bad as = do
    case as of
        [] -> return []
        (a:as) -> do
            when (elem a bad) $ throwError (show a ++ " benne volt a tiltott elemek listájában, a számítást nem lehet elvégezni")
            as' <- performOperations f bad as
            return ((f a):as')