module MTFHazi2 where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe

-- Minden feladat során az alábbi stackben fogunk dolgozni
-- Állapotváltozási környezet, benne egy [(String, String)] típusu értékkel (felhasználónév és jelszó listapár)
-- Olvasási környezet, benne egy (String, String) típusú értékkel (felhasználónév és home directory)
-- Írási környezet, benne egy [String] típusú értékkel (üzenetek logja)
-- Hibakezelési környezet, String típusú hibaüzenetekkel

-- Lehet megkötéseket a feladat során használni, csak itt kell explicit a sorrendet megadni
runStackToIO :: StateT [(String, String)] (ReaderT (String, String) (WriterT [String] (ExceptT String IO))) a -> IO (Either String ((a, [(String, String)]), [String]))
runStackToIO m = runExceptT (runWriterT (runReaderT (runStateT m [("a", "p"),("b", "q"),("c", "r"),("d", "s")]) ("root", "/")))

type MonadStack m = (MonadState [(String, String)] m, MonadReader (String, String) m, MonadWriter [String] m, MonadError String m, MonadIO m)

-- Definiálj egy createUser függvényt, amely egy felhasználónevet és egy jelszót beolvas STDIN-ról, majd hozzáadja azt a felhasználók listájához.
createUser :: MonadStack m => m ()
createUser = do
    user <- liftIO getLine
    pw <- liftIO getLine
    modify ((user, pw):)

-- Definiálj egy adminCheck függvényt, amely megvizsgálja, hogy a jelenlegi felhasználó admin-e. Ha nem, írja ki a logba, hogy "Non-Admin User" és dobjon tetszőleges tartalmú hibaüzenetet.
adminCheck :: MonadStack m => m ()
adminCheck = do
    (user, _) <- ask
    if user /= "root" then do 
        tell ["Non-Admin User"]
        throwError "Non-Admin User"
    else return () 

--Definiálj egy deleteUser függvényt, amely egy paraméterül kapott nevű felhasználót kitöröl az állapotból. Előfeltételként vizsgáljuk meg, hogy a felhasználó admin-e az előző függvény segítségével. Ha a kitörlendő felhasználó nem létezik, írjuk a logba.
deleteUser :: MonadStack m => String -> m ()
deleteUser user = do
    adminCheck
    users <- get
    case lookup user users of
        Nothing -> tell ["User does not exist"]
        (Just pw) -> modify (delete (user, pw))