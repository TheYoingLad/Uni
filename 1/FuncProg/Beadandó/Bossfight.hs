module Boss where
import Data.List
import Data.Maybe

showState a = show a
showMage a = show a
eqMage a b =  a == b
showUnit a = show a
showOneVOne a = show a

type Name = String
type Health = Integer
type Spell = (Integer -> Integer)
type Army = [Unit]
type EnemyArmy = Army
type Amount = Integer

------- Mesterek ---------

papi = let 
    tunderpor enemyHP
        | enemyHP < 8 = 0
        | even enemyHP = div (enemyHP * 3) 4
        | otherwise = enemyHP - 3
    in Master "Papi" 126 tunderpor
java = Master "Java" 100 (\x ->  x - (mod x 9))
traktor = Master "Traktor" 20 (\x -> div (x + 10) ((mod x 4) + 1))
jani = Master "Jani" 100 (\x -> x - div x 4)
skver = Master "Skver" 100 (\x -> div (x+4) 2)
potionMaster = let 
    plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
    in Master "PotionMaster" 170 plx

--------Segéd függvények-----------

getHealth :: Unit -> Integer
getHealth x = case x of
    (E Dead) -> 0
    (M Dead) -> 0
    (E (Alive (Golem x))) -> x
    (E (Alive (HaskellElemental x))) -> x
    (M (Alive (Master _ x _))) -> x

isDead :: Unit -> Bool
isDead = (>=) 0 . getHealth

toDead :: Unit -> Unit
toDead x = case x of
    e@(E _) -> if isDead e then (E Dead) else e
    m@(M _) -> if isDead m then (M Dead) else m

damage :: (Integer -> Integer) -> Unit -> Unit
damage f x = case x of
    (E Dead) -> E Dead
    (M Dead) -> M Dead
    (E (Alive (Golem x))) -> toDead(E $ Alive $ Golem $ f x)
    (E (Alive (HaskellElemental x))) -> toDead(E $ Alive $ HaskellElemental $ f x)
    (M (Alive (Master n x s))) -> toDead(M $ Alive $ Master n (f x) s)

---------------------------MAIN--------------------------

data State a = Alive a | Dead
    deriving Eq

instance Show a => Show (State a) where
    show (Alive x) = show x
    show (Dead) = "Dead"

data Entity = Golem Health | HaskellElemental Health
    deriving(Eq, Show)

data Mage = Master Name Health Spell

instance Show Mage where
    show (Master x y _)
        | y >= 5 = x
        | otherwise = "Wounded " ++ x

instance Eq Mage where
    (==) (Master x1 y1 _) (Master x2 y2 _) = x1 == x2 && y1 == y2

data Unit = M (State Mage) | E (State Entity)
    deriving Eq

instance Show Unit where
    show (M x) = show x
    show (E x) = show x

formationFix :: Army -> Army
formationFix xs = filter f xs ++ filter (not . f) xs where
    f x = case (toDead x) of {(M Dead) -> False; (E Dead) -> False; _ -> True}

over :: Army -> Bool
over xs = and $ map f xs where
    f x = case x of {(M Dead) -> True; (E Dead) -> True; _ -> False}

fight :: EnemyArmy -> Army -> Army
fight [] ys = ys
fight _ [] = []
fight (x:xs) (y:ys) = case x of
    (E Dead) -> y : fight xs ys
    (M Dead) -> y : fight xs ys
    (E (Alive (Golem _))) -> (damage (+(-1)) y) : (fight xs ys)
    (E (Alive (HaskellElemental _))) -> (damage (+(-3)) y) : (fight xs ys)
    (M (Alive (Master _ _ f))) -> (damage f y) : (fight xs (map (damage f) ys))

haskellBlast :: Army -> Army
haskellBlast xs = map toDead (take a xs) ++ map (damage (+(-5))) (take 5 (drop a xs)) ++ map toDead (drop (a+5) xs) where
    a = findBlast deltaDamage
    findBlast xs = fromMaybe (0) $ findIndex ((==) $ maximum xs) xs
    take5Sum [] = []
    take5Sum a@(x:xs) = sum(take 5 a) : take5Sum xs
    deltaDamage = take5Sum (zipWith (-) currentHealt damagedHealth)
    currentHealt = map getHealth xs
    damagedHealth = map getHealth (map (damage (+(-5))) xs)

multiHeal :: Health -> Army -> Army
multiHeal _ [] = []
multiHeal n xs
    | n <= 0 = xs
    | over xs = xs
    | aliveMaxN n xs == xs = multiHeal (n - (fromIntegral $ healed xs)) $ map (damage (+1)) xs
    | otherwise = subheal n xs where
        healed xs = length $ filter (/=0) $ map (getHealth . toDead) xs
        subheal 0 xs = xs
        subheal _ [] = []
        subheal n (x:xs) = if isDead x then x : (subheal n xs) else (damage (+1) x) : (subheal (n-1) xs)
        aliveMaxN _ [] = []
        aliveMaxN 0 xs = []
        aliveMaxN n (x:xs) = if isDead x then x : (aliveMaxN n xs) else x : (aliveMaxN (n-1) xs)

--------------------------EXTRA----------------------------

battle :: Army -> EnemyArmy -> Maybe Army
battle xs ys
    | over xs && over ys = Nothing
    | over xs = Just ys
    | over ys = Just xs
    | otherwise = battle (formationFix $ multiHeal 20 $ haskellBlast $ fight ys xs) (formationFix $ fight xs ys)

chain :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
chain n (xs, ys) = (fst done ++ remainderXs, snd done ++ remainderYs) where
    xsPre = map toDead xs
    ysPre = map toDead ys
    remainderXs= case (flip drop xsPre $ length main) of
        [] -> []
        (x:xs) -> if n - getN main > 0 then (damage (+(n - getN main)) x):xs else (x:xs)
    remainderYs = flip drop ysPre $ length main
    main = zip xsPre ysPre
    done = unzip $ chainDamage n main
    getN xs = fromIntegral((length (filter (not . isDead) (fst double))) + (length (filter (not . isDead) (snd double)))) where
        double = unzip xs
    chainDamage _ [] = []
    chainDamage n (x:xs)
        | n <= 0 = (x:xs)
        | (&&) (isDead $ fst x) (isDead $ snd x) = x:(chainDamage n xs)
        | isDead $ fst x = (fst x, damage (+(-n)) $ snd x):(chainDamage (n-1) xs)
        | isDead $ snd x = (damage (+n) $ fst x, snd x):(chainDamage (n-1) xs)
        | otherwise = (damage (+n) $ fst x, damage (+(1-n)) $ snd x):(chainDamage (n-2) xs)

battleWithChain :: Army -> EnemyArmy -> Maybe Army
battleWithChain xs ys
    | over xs && over ys = Nothing
    | over xs = Just ys
    | over ys = Just xs
    | otherwise = battleWithChain (formationFix postXs) (formationFix postYs) where
        preXs = multiHeal 20 $ haskellBlast $ fight ys xs
        preYs = fight xs ys
        (postXs, postYs) = chain 5 (preXs, preYs)

data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne
    deriving Eq

instance Show OneVOne where
    show (Winner x) = "<|| Winner " ++ x ++ " ||>"
    show (You n x) = "<You " ++ show n ++ "; " ++ (drop 1 $ show x)
    show (HaskellMage n x) = "<HaskellMage " ++ show n ++ "; " ++ (drop 1 $ show x)

finalBattle :: Health -> Health -> OneVOne
finalBattle n m = haskellTurn n m where
    haskellTurn n m
        | m <= 0 = HaskellMage 0 (Winner "You")
        | m < 4 = HaskellMage m (yourTurn (div n 2) (4*m))
        | n > 20 = HaskellMage m (yourTurn (div (3*n) 4) m)
        | otherwise = HaskellMage m (yourTurn (n-11) m)
    yourTurn n m
        | n <= 0 = You 0 (Winner "HaskellMage")
        | n < 4 = You n (haskellTurn (4*n) m)
        | m > 15 = You n (haskellTurn n (div (3*m) 5))
        | otherwise = You n (haskellTurn n (m-9))