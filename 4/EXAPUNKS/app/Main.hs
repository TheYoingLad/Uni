{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Solver where
import Data.Maybe
import Data.List
import Data.Either
import Data.Char
import System.IO
import Data.Hashable

-------------- DATA TYPES -----------------------

data Suit = Hearts | Diamonds | Clubs | Spades deriving Eq
instance Show Suit where
    show Hearts = "H"
    show Diamonds = "D"
    show Clubs = "C"
    show Spades = "S"

data Face = Ace | King | Queen | Jack deriving Eq
instance Show Face where
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"

data Card = Face Face Suit | Number Int Suit deriving Eq
instance Show Card where
    show (Face f c) = intercalate "-" [show f, show c]
    show (Number n c) = intercalate "-" [show n, show c]

--              --head, top of the group
--              V             V-tail, bottom of the group
type Group = (Card, [Card], Card)

type Deck = Either [Group] Group

type Table = ([Deck], Maybe Card)

--------------------------------------------

----------- HELPER FUNCTIONS ---------------

head' :: [a] -> a
head' = fst . fromJust . uncons

-- tail' :: [a] -> a
-- tail' = snd . fromJust . uncons

fromLeft' :: Either a b -> a
fromLeft' = fromLeft undefined

fromRight' :: Either a b -> b
fromRight' = fromRight undefined

getColor :: Suit -> Bool
getColor Diamonds = True
getColor Hearts = True
getColor Clubs = False
getColor Spades = False

cardToGroup :: Card -> Group
cardToGroup card = (card, [card], card)

-----------------------------------------------

------------------- MAIN LOGIC ---------------------

--           top   bottom
canMerge :: Card -> Card -> Bool
canMerge (Face _ _) (Number _ _) = False
canMerge (Number _ _) (Face _ _) = False
canMerge (Face _ s1) (Face _ s2) = s1 == s2
canMerge (Number n1 s1) (Number n2 s2) = n1 + 1 == n2 && getColor s1 /= getColor s2

--             top     bottom
mergeGroup :: Group -> Group -> Maybe Group
mergeGroup (topDeckTop, topDeck, topDeckBottom) (bottomDeckTop, bottomDeck, bottomDeckBottom)
    | canMerge topDeckBottom bottomDeckTop = Just (topDeckTop, topDeck ++ bottomDeck, bottomDeckBottom)
    | otherwise = Nothing

mergeDeck :: Deck -> Deck
mergeDeck (Right _) = undefined
mergeDeck x@(Left []) = x
mergeDeck x@(Left [_]) = x
mergeDeck (Left (top:bottom:rest)) = case mergeGroup top bottom of
    Nothing -> Left (top : fromLeft' (mergeDeck $ Left (bottom : rest)))
    Just result -> mergeDeck $ Left (result : rest)

removeTopFromGroup :: Group -> (Card, Maybe Group)
--removeTopFromGroup (_, [], _) = (Number (-1) Spades, Nothing)
removeTopFromGroup (top, [_], _) = (top, Nothing)
removeTopFromGroup (top, _:newTop:rest, bottom) = (top, Just (newTop, newTop:rest, bottom))

--                      done     to do
takeFirstsFromDecks :: [Deck] -> [Deck] -> [Table]
takeFirstsFromDecks _ [] = []
takeFirstsFromDecks done (next@(Right _):rest) = takeFirstsFromDecks (done ++ [next]) rest
takeFirstsFromDecks done (next@(Left []):rest) = takeFirstsFromDecks (done ++ [next]) rest
takeFirstsFromDecks done (next@(Left (group:groups)):rest) = do
    let (topCard, nextGroupWrapper) = removeTopFromGroup group
    case nextGroupWrapper of
        Nothing -> (done ++ (Left groups : rest), Just topCard) : takeFirstsFromDecks (done ++ [next]) rest
        Just nextGroup -> (done ++ (Left (nextGroup : groups) : rest), Just topCard) : takeFirstsFromDecks (done ++ [next]) rest

isDone :: Group -> Bool
isDone (Number _ _, cards, _) = length cards == 5
isDone (Face _ _ , cards, _) = length cards == 4

--                  done     to do      put        slot    exclude
putToTopOfDecks :: [Deck] -> [Deck] -> Group -> Maybe Card-> Int -> [Table]
putToTopOfDecks _ [] _ _ _ = []
putToTopOfDecks done (next@(Right _):rest) group slot exclude = putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
putToTopOfDecks done (next@(Left []):rest) group slot exclude
    | exclude == 0 = putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
    | isDone group = (done ++ (Right group : rest), slot) : putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
    | otherwise = (done ++ (Left [group] : rest), slot) : putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
putToTopOfDecks done (next@(Left (oldGroup:groups)):rest) group slot exclude
    | exclude == 0 = putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
    | otherwise = case mergeGroup group oldGroup of
    Just nextGroup -> if null groups && isDone nextGroup
        then (done ++ (Right nextGroup : rest), slot) : putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
        else (done ++ (Left (nextGroup : groups) : rest), slot) : putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)
    Nothing -> putToTopOfDecks (done ++ [next]) rest group slot (exclude - 1)

putAllGroups :: [Deck] -> Maybe Card -> [Table]
putAllGroups decks slot = concat $ do
    n <- [0..8]
    case decks !! n of
        Right _ -> return []
        Left groups -> case uncons groups of
            Nothing -> return []
            Just (topGroup, restGroups) -> return $ putToTopOfDecks [] (take n decks ++ (Left restGroups : drop (n + 1) decks)) topGroup slot n

steps :: Table -> [Table]
--steps (decks, Nothing) = putAllGroups decks Nothing ++ takeFirstsFromDecks [] decks
steps (decks, Nothing) = putAllGroups decks Nothing
steps (decks, Just card) = putAllGroups decks (Just card) ++ putToTopOfDecks [] decks (cardToGroup card) Nothing (-1)

stepAll :: [Table] -> [Table]
--stepAll = nub . concatMap steps
stepAll = filter (\(decks, _) -> any isRight decks) . nub . concatMap steps
---------------------------------------------------------------

-------------------- READ FROM FILE ---------------------------

stringToCard :: String -> Card
stringToCard [faceNum, suit]
    | isNumber faceNum = Number (charToInt faceNum) (charToSuit suit)
    | otherwise = Face (charToFace faceNum) (charToSuit suit) where
    charToInt :: Char -> Int
    charToInt '0' = 10
    charToInt '9' = 9
    charToInt '8' = 8
    charToInt '7' = 7
    charToInt '6' = 6
    charToInt _ = undefined
    charToFace :: Char -> Face
    charToFace 'J' = Jack
    charToFace 'Q' = Queen
    charToFace 'K' = King
    charToFace 'A' = Ace
    charToFace _ = undefined
    charToSuit :: Char -> Suit
    charToSuit 'D' = Diamonds
    charToSuit 'H' = Hearts
    charToSuit 'C' = Clubs
    charToSuit 'S' = Spades
    charToSuit _ = undefined
stringToCard _ = undefined

cardsToDecks :: [Card] -> [Deck]
cardsToDecks (c1:c2:c3:c4:rest) = Left (map cardToGroup [c1,c2,c3,c4]) : cardsToDecks rest
cardsToDecks [] = []
cardsToDecks _ = undefined

parse :: IO ()
parse = do
    raw_data <- readFile "input.txt"
    let decks = map mergeDeck $ cardsToDecks $ map stringToCard $ words raw_data
    let table = (decks, Nothing)
    --let modifiedTables = stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll $ stepAll [table]
    let modifiedTables = stepAll [table]
    --prettyPrintTable table
    prettyPrintTable $ head' modifiedTables
    --print $ length modifiedTables
    --prettyWriteTables modifiedTables "output.txt"
    --print $ head' modifiedTables
    --prettyPrintTable $ fromJust $ find (\(decks, _) -> any isRight decks) modifiedTables
    print $ find (\(decks, _) -> all isRight decks) modifiedTables
    --print $ length $ take 1000000 modifiedTables
    --prettyPrintTable (modifiedTables !! 222)
    --print (modifiedTables !! 222)
    --prettyPrintTable (modifiedTables !! 10000)
    --print (modifiedTables !! 10000)


--type Table' = (Deck, Deck, Deck, Deck, Deck, Deck, Deck, Deck, Deck, Maybe Card)

---------------------------------------------------------------------

----------------------------------- PRINT ---------------------------



prettyPrintTableRows :: [[Card]] -> IO ()
prettyPrintTableRows cardss = if all null cardss then return () else do
    let (row, rest) = getRow cardss
    putStrLn row
    prettyPrintTableRows rest where
        getRow :: [[Card]] -> (String, [[Card]])
        getRow cardss = ("|" ++ concatMap (getString . take 1) cardss,  map (drop 1) cardss)
        getString :: [Card] -> String
        getString [] = "\t\t|"
        getString [card] = "\t" ++ show card ++ "\t|"

getCardsFromDeck :: Deck -> [Card]
getCardsFromDeck (Right (_, cards, _)) = reverse cards
getCardsFromDeck (Left groups) = reverse $ concatMap (\(_, cards, _) -> cards) groups

prettyPrintTableCell :: Maybe Card -> IO ()
prettyPrintTableCell (Just card) = putStrLn $ concat ["| " , show card, " |"]
prettyPrintTableCell Nothing = putStrLn "|  |"

prettyPrintTable :: Table -> IO ()
prettyPrintTable (decks, cell) = do
    prettyPrintTableCell cell
    prettyPrintTableRows $ map getCardsFromDeck decks
    putStrLn $ replicate 150 '-'

prettyPrintTables :: [Table] -> IO ()
prettyPrintTables [] = return ()
prettyPrintTables (next:rest) = do
    prettyPrintTable next
    prettyPrintTables rest

---------------------------------------------------------------------

prettyWriteTableRows :: [[Card]] -> Handle -> IO ()
prettyWriteTableRows cardss handle = if all null cardss then return () else do
    let (row, rest) = getRow cardss
    hPutStrLn handle row
    prettyWriteTableRows rest handle where
        getRow :: [[Card]] -> (String, [[Card]])
        getRow cardss = ("|" ++ concatMap (getString . take 1) cardss,  map (drop 1) cardss)
        getString :: [Card] -> String
        getString [] = "\t\t\t|"
        getString [card@(Number 10 _)] = "\t" ++ show card ++ "\t|"
        getString [card] = "\t" ++ show card ++ "\t\t|"

prettyWriteTableCell :: Maybe Card -> Handle -> IO ()
prettyWriteTableCell (Just card) handle = hPutStrLn handle $ concat ["| " , show card, " |"]
prettyWriteTableCell Nothing handle = hPutStrLn handle "|  |"

prettyWriteTable :: Table -> Handle -> IO ()
prettyWriteTable (decks, cell) handle = do
    prettyWriteTableCell cell handle
    prettyWriteTableRows (map getCardsFromDeck decks) handle
    hPutStrLn handle $ replicate 110 '-'
    hPutStrLn handle ""

prettyWriteTablesHelper :: [Table] -> Handle -> IO ()
prettyWriteTablesHelper [] _ = return ()
prettyWriteTablesHelper (next:rest) handle = do
    prettyWriteTable next handle
    prettyWriteTablesHelper rest handle

prettyWriteTables :: [Table] -> String -> IO ()
prettyWriteTables tables path = do
    writeFile path ""
    handle <- openFile path AppendMode
    hSetBuffering handle LineBuffering
    prettyWriteTablesHelper tables handle
    hClose handle