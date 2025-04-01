module PwZ where
import Data.List
import Data.Maybe

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int
    deriving(Eq, Show)

data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
    deriving(Eq, Show)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)]
    deriving(Eq, Show)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

---------------------------------------------------------------------------

cost = [(defaultWalnut, 50),(defaultSunflower, 50),(defaultPeashooter, 100),(defaultCherryBomb, 150)]
getCoord = fst . unzip
getPlant = snd . unzip
getRow = fst . unzip . getCoord
getCol = snd . unzip . getCoord
getSpeedHelp (Basic _ x) = x
getSpeedHelp (Conehead _ x) = x
getSpeedHelp (Buckethead _ x) = x
getSpeedHelp (Vaulting _ x) = x
getSpeed = map getSpeedHelp . snd . unzip
isEnd zs = null $ filter (<0) $ zipWith (-) (getCol zs) (getSpeed zs)
collide ps zs = zipWith (==) (getCoord ps) (getCoord zs)
hpDown (Walnut x) = Walnut (x-1)
hpDown (Sunflower x) = Sunflower (x-1)
hpDown (Peashooter x) = Peashooter (x-1)
movLeft z = ((fst(fst z), ((snd(fst z))-1)), snd z)
biteHelp [] z = []
biteHelp (p:ps) z
    | (fst z) == (fst p) = ((fst p), (hpDown (snd p))):(biteHelp ps z)
    | otherwise = p:(biteHelp ps z)
bite ps [] = ps
bite ps (z:zs) = bite (biteHelp ps z) zs
move ps [] = []
move ps (z:zs)
    | elem (fst z) (getCoord ps) = z:(move ps zs)
    | otherwise = (moveLeft z):zs

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun ps zs) coord@(row,col) p
    | row < 0 || row > 4 || col < 0 || col > 11 = Nothing
    | elem coord (getCoord ps) = Nothing
    | (fromJust $ lookup p cost) > sun = Nothing
    | otherwise = Just (GameModel (sun-(fromJust $ lookup p cost)) ((coord, p):ps) zs)

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun ps zs) z row
    | row < 0 || row > 4 = Nothing
    | elem (row, 11) (getCoord zs) = Nothing
    | otherwise = Just (GameModel sun ps (((row, 11), z):zs))

performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel sun ps zs)
    | not $ isEnd zs = Nothing
    | or $ collide ps zs = Nothing