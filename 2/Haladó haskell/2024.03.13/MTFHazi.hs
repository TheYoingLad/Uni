module MTFHazi where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.List
import Data.Maybe

-- Vegyük az alábbi környezetet
data Env = MkEnv {
  isRootUser :: Bool, -- Root-e a felhasználó
  userName   :: String, -- felhasználónév
  sudoers    :: [String], -- azon felhasználónevek, akiknek van admin engedélyük
  users      :: [(String, String)], -- felhasználónevek és jelszók
  pwd        :: String, -- jelenlegi mappa ami nyitva van, munkadirectory
  homeDir    :: String  -- a felhasználó homedirectoryja
                 }

instance Show Env where
  show (MkEnv isRoot u ss us pwd home) = unwords[show isRoot, u, show ss, show us, pwd, home]

testReader :: Reader Env Env
testReader = ask

defaultEnv = MkEnv False "user" ["sudo1", "sudo2"] [("a", "b"), ("user", "pass")] "/home/Downloads" "/kekbanan"

-- READER
-- Definiáljuk az isSudoable függvényt, amely megnézi, hogy a jelen felhasználónak lehet-e admin engedélye.
-- Akkor lehet engedélye, ha jelenleg root felhasználól vagyunk, vagy a sudoers listában benne van a felhasználó neve.
isSudoable :: Reader Env Bool
isSudoable = do
  MkEnv isRoot user sudo _ _ _ <- ask
  return (isRoot || elem user sudo)

-- Definiáljuk a getPassword függvény amely visszaadja a felhasználó jelszavát (ha van)
getPassword :: Reader Env (Maybe String)
getPassword = do
  MkEnv _ u _ us _ _ <- ask
  case [b | (a,b) <- us, a == u] of
    [] -> return Nothing
    [x] -> return (Just x)

-- Definiáljuk a runAs függvényt amely megpróbál egy Readert egy felhasználónként lefuttatni.
-- Az első paraméter a felhasználó név a második a jelszó. Ha nincs ilyen felhasználó vagy a jelszó nem helyes adjunk vissza nothingot
runAs :: String -> String -> Reader Env a -> Reader Env (Maybe a)
runAs u p r = do
  MkEnv _ u _ us _ _ <- ask
  case [(a,b) | (a,b) <- us, a == u, b == p] of
    [] -> return Nothing
    _ -> do
      r' <- r
      return (Just r')

-- Definiáljuk az inHomeDir függvényt amely egy readert lefuttat úgy, hogy a munkadirectory a felhasználó homedirectoryja legyen
inHomeDir :: Reader Env a -> Reader Env a
inHomeDir r = do
  MkEnv a b c d _ home <- ask
  local (const (MkEnv a b c d home home)) r

-- Definiáljuk a defineUsers amely felhasználókat definiál az adott névvel és jelszókkal.
-- Ha egy felhasználó már létezik és jelenleg adminok vagyunk, akkor a nem admin felhasználókat írjuk fölül, egyébként azokat ignoráljuk.
defineUsers :: [(String, String)] -> Reader Env a -> Reader Env a
defineUsers xs r = do
  case xs of
    [] -> r
    (a@(name, pw):as) -> do
      MkEnv isRoot p1 sudos users p2 p3 <- ask
      case null[u | (u,p) <- users, name == u] of
        True ->  local (const $ MkEnv isRoot p1 sudos (a:users) p2 p3) (defineUsers as r)
        False -> do
          case isRoot of
            False -> local (const $ MkEnv isRoot p1 sudos users p2 p3) (defineUsers as r)
            True -> do
              case elem name sudos of
                True -> local (const $ MkEnv isRoot p1 sudos users p2 p3) (defineUsers as r)
                False -> local (const $ MkEnv isRoot p1 sudos (replace a users) p2 p3) (defineUsers as r) where
                  replace _ [] = []
                  replace x@(nev,jel) (fel@(felnev,feljel):fels) | felnev == nev = (nev,jel):(replace x fels)
                                                                 | otherwise = fel:(replace x fels)

-- WRITER

testWriter :: Int -> Writer [String] ()
testWriter n = do
  tell [show n]
  when (n > 0) $ testWriter (n-1)

-- Vegyük az alábbi típusokat
type IPAdress = (Int, Int, Int, Int) -- mindegyik szám 0 és 255 között van és egy IP cím egy oktetét jelzi decimálisan, pl 127.0.0.1 = (127, 0, 0, 1)
data Action = LogIn | LogOut | DoMagic deriving (Eq, Show)    

-- Definiáljuk az xMagic függvényt, amely a 127.0.0.1-es IP címre loggol X darab DoMagic akciót
xMagic :: Int -> Writer [(IPAdress, Action)] ()
xMagic 0 = return ()
xMagic n = do
  tell [((127, 0, 0, 1), DoMagic)]
  xMagic $ n-1

testMagic = runWriter (xMagic 4)

-- Definiáljuk a logAround függvény, amely egy IP címet kap paraméterül, loggol egy belépést az adott IP címmel, lefuttatja a második paraméterben kapott writert
-- majd loggol egy kilépést az adott IP címmel
logAround :: IPAdress -> Writer [(IPAdress, Action)] a -> Writer [(IPAdress, Action)] a
logAround ip  writer = do
  tell [(ip, LogIn)]
  a <- writer
  tell [(ip, LogOut)]
  return a

testLogAround = runWriter (logAround (0,0,0,0) $ xMagic 4)

-- Definiáljuk a countMagic függvényt, amely megszámolja hogy a paraméterül kapott writerbe melyik IP cím loggolta a legtöbb DoMagic actiont
countMagic :: Writer [(IPAdress, Action)] () -> Writer [(IPAdress, Action)] IPAdress
countMagic writer = do
  logs <- listen writer
  return $ result logs where
    getIP xs = map fst $ filter ((==DoMagic) . snd) $ snd xs
    count x xs = foldr (\a acc -> if a == x then acc + 1 else acc) 0 xs
    result xs = fromJust $ do
      ind <- maximumIndex[n | ip <- nub $ getIP xs, let n = count ip  $ getIP xs]
      return ((nub $ getIP xs)!!ind)
    maximumIndex xs = findIndex ((==) $ maximum xs) xs

testCountMagic = runWriter (countMagic $ logAround (0,0,0,0) $ xMagic 2)

-- Definiáljuk a trySudo függvényt, amely lefuttatja a paraméterül kapott műveletet, ha a felhasználó admin / benne van a sudoers fileban
-- Ha nincs ne futassuk le a műveletet és írjuk a logba hogy "$X is not in the sudoers file, this incident will be reported"
-- A környezetet bemeneti paraméterként kapjuk
trySudo :: Env -> Writer [String] () -> Writer [String] ()
trySudo (MkEnv isRoot u ss _ _ _) w = do
  if (isRoot || elem u ss) then w else tell [u ++ " is not in the sudoers file, this incident will be reported"]

testTrySudo = runWriter (trySudo testSudoEnv $ testWriter 3)
testSudoEnv = MkEnv True "user" ["sudo1", "sudo2"] [("a", "b"), ("user", "pass")] "/home/Downloads" "/kekbanan"

-- EXCEPT
-- Definiáljuk a safeFactorial függvényt amely rekurzívan elvégzi a faktoriális függvényt. Ha a paraméterül kapott szám < 0 dobjunk errort!
safeFactorial :: Int -> Except String Int
safeFactorial n 
  | n < 0 = throwError "0-nál kisebb számra nincs értelmezve!"
  | n == 0 = return 1
  | n == 1 = return n
  | otherwise = do 
    next <- safeFactorial (n-1)
    return (n * next)

testSafeFactorial n = runExcept (safeFactorial n)

-- Definiáljuk a lessSafeFactorial függvényt amely azon értékekre, amelyekre a safeFactorial error-t dobna, adjon vissza -1-et.
-- Ehhez használjuk fel az előző függvényt és a catchError függvényt.
lessSafeFactorial :: Int -> Except String Int
lessSafeFactorial n = do
  catchError (safeFactorial n) (\_ -> return (-1))

testLessSafeFactorial n = runExcept (lessSafeFactorial n)

-- Definiáljuk újra a trySudo függvényt, de loggolás helyett dobjunk errort ha nem admin a felhaszáló
trySudoE :: Env -> Except String a -> Except String a
trySudoE (MkEnv isRoot u ss _ _ _) e = do
  if (isRoot || elem u ss) then e else throwError (u ++ " is not in the sudoers file, this incident will be reported")

testTrySudoE = runExcept (trySudoE testSudoEnvE $ lessSafeFactorial 3)
testSudoEnvE = MkEnv False "user" ["sudo1", "sudo2"] [("a", "b"), ("user", "pass")] "/home/Downloads" "/kekbanan"