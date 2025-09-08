{-# LANGUAGE LambdaCase #-}
module Exapunks where
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad (guard)

data Card = Face Face Suit | Number Int Suit deriving Eq
instance Show Card where
    show (Face f c) = show f ++ " of " ++ show c
    show (Number n c) = show n ++ " of " ++ show c
    
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

type Deck = [Card]
type Table = ([Deck], Maybe Card)

testCard1 :: Card
testCard1 = Number 10 Hearts
testCard2 :: Card
testCard2 = Number 9 Clubs
testCard3 :: Card
testCard3 = Number 8 Diamonds
testCard4 :: Card
testCard4 = Number 7 Spades
testCard5 :: Card
testCard5 = Number 6 Hearts
testCard6 :: Card
testCard6 = Number 6 Spades
testCard7 :: Card
testCard7 = Face Ace Hearts
testCard8 :: Card
testCard8 = Face King Hearts
testCard9 :: Card
testCard9 = Face Queen Hearts
testCard10 :: Card
testCard10 = Face Jack Hearts
testCard11 :: Card
testCard11 = Face Ace Diamonds

testDeck1 :: Deck
testDeck1 = [testCard5, testCard4, testCard3, testCard2, testCard1]
testDeck2 :: Deck
testDeck2 = [testCard6, testCard1, testCard2, testCard3, testCard4, testCard6]
testDeck3 :: Deck
testDeck3 = [testCard7, testCard8, testCard9, testCard10]
testDeck4 :: Deck
testDeck4 = [testCard7, testCard8, testCard9, testCard11]
testDeck5 :: Deck
testDeck5 = [testCard1, testCard8, testCard3,testCard10]

testTable1 :: Table
testTable1 = ([testDeck1, testDeck2, testDeck3, testDeck4, testDeck5, testDeck1, testDeck3, [], reverse testDeck2], Nothing)
testTable2 :: Table
testTable2 = ([[Number 6 Hearts, Number 9 Clubs, Number 9 Diamonds, Face Jack Clubs], 
               [Number 10 Spades, Number 7 Spades, Face Queen Hearts, Number 6 Clubs], 
               [Number 8 Diamonds, Face Ace Hearts, Number 7 Hearts, Face Ace Diamonds], 
               [Face Jack Hearts, Face Jack Spades, Number 10 Clubs, Number 8 Clubs], 
               [Face King Spades, Face King Clubs, Number 10 Hearts, Face Queen Clubs], 
               [Number 7 Clubs, Face Jack Diamonds, Number 9 Hearts, Number 6 Diamonds], 
               [Number 10 Diamonds, Face Ace Spades, Face Queen Spades, Face Queen Diamonds], 
               [Number 6 Spades, Face King Diamonds, Face Ace Clubs, Number 8 Hearts], 
               [Face King Hearts, Number 7 Diamonds, Number 8 Spades, Number 9 Spades]], 
               Nothing)
            
testTable3 :: Table
testTable3 = ([[Number 9 Clubs, Number 9 Diamonds, Face Jack Clubs], 
               [Number 10 Spades, Number 7 Spades, Face Queen Hearts, Number 6 Clubs], 
               [Number 8 Diamonds, Face Ace Hearts, Number 7 Hearts, Face Ace Diamonds], 
               [Face Jack Hearts, Face Jack Spades, Number 10 Clubs, Number 8 Clubs], 
               [Face King Spades, Face King Clubs, Number 10 Hearts, Face Queen Clubs], 
               [Number 7 Clubs, Face Jack Diamonds, Number 9 Hearts, Number 6 Diamonds], 
               [Number 10 Diamonds, Face Ace Spades, Face Queen Spades, Face Queen Diamonds], 
               [Number 6 Spades, Face King Diamonds, Face Ace Clubs, Number 8 Hearts], 
               [Face King Hearts, Number 7 Diamonds, Number 8 Spades, Number 9 Spades]], 
               Just $ Number 6 Hearts)

---------------------------VALIDATE-------------------------------------

allOfSuit :: Suit -> Deck
allOfSuit suit = [Face Ace suit, Face King suit, Face Queen suit, Face Jack suit] ++ [Number n suit | n <- [10, 9.. 6]]

hasAllOfSuit :: [Card] -> Suit -> Bool
hasAllOfSuit cards suit = all (\card -> elem card cards) $ allOfSuit Hearts

validateDecks :: [Deck] -> Bool
validateDecks decks = length cards == 36 && hasAllOfSuit cards Hearts && hasAllOfSuit cards Diamonds && hasAllOfSuit cards Clubs && hasAllOfSuit cards Spades where
    cards = concat decks

validateTable :: Table -> Bool
validateTable (decks, Just card) = validateTable ([card] : decks, Nothing)
validateTable (decks, _) = validateDecks decks

---------------------------IS SOLVED-------------------------------------

cardToSuit :: Card -> Suit
cardToSuit (Face _ s) = s
cardToSuit (Number _ s) = s

suitToBool :: Suit -> Bool
suitToBool Hearts = True
suitToBool Diamonds = True
suitToBool Clubs = False
suitToBool Spades = False

cardToBool :: Card -> Bool
cardToBool = suitToBool . cardToSuit

isAlternatingColour :: Deck -> Bool
isAlternatingColour [] = True
isAlternatingColour [_] = True
isAlternatingColour (card1 : card2 : deck)
    | cardToBool card1 /= cardToBool card2 = isAlternatingColour (card2 : deck) 
    | otherwise = False

deckToNumbers :: Deck -> [Int]
deckToNumbers [] = []
deckToNumbers ((Number n _) : deck) = n : (deckToNumbers deck)

isAllNumbers :: Deck -> Bool
isAllNumbers = all (\case {Number _ _ -> True ; _ -> False})

isAllFaces :: Deck -> Bool
isAllFaces = all (\case {Face _ _ -> True ; _ -> False})

isSolvedDeck :: Deck -> Bool
isSolvedDeck [] = False
isSolvedDeck deck@((Number _ _) : _) = isAllNumbers deck && deckToNumbers deck == [6, 7, 8, 9, 10] && isAlternatingColour deck
isSolvedDeck deck@((Face _ s) : _) = isAllFaces deck && all ((s ==) . cardToSuit) deck && length deck == 4

isSolvedTable :: Table -> Bool
isSolvedTable (_, Just _) = False
isSolvedTable (decks, _) = all isSolvedDeck decks

---------------------------PRINT-------------------------------------

prettyPrintCell :: Maybe Card -> IO ()
prettyPrintCell (Just card) = putStrLn $ concat ["| " , show card, " |"]
prettyPrintCell Nothing = putStrLn "|  |"

hasNonEmptyDeck :: [Deck] -> Bool
hasNonEmptyDeck = any (not . null)

head' :: [a] -> a
head' = fst . fromJust . uncons

last' :: [a] -> a
last' = snd . fromJust . unsnoc

tail' :: [a] -> [a]
tail' = snd . fromJust . uncons

getRowString :: [Deck] -> String
getRowString = intercalate "\t\t|\t" . foldr (\x acc -> if null x then "" : acc else (show $ head' x) : acc) []

getNewDeck :: [Deck] -> [Deck]
getNewDeck = map (\deck -> if null deck then deck else tail' deck)

printHelper :: [Deck] -> (String, [Deck])
printHelper decks = (getRowString decks , getNewDeck decks)

prettyPrintTableRows :: [Deck] -> IO ()
prettyPrintTableRows decks = if hasNonEmptyDeck decks then do
        let (s, newDecks) = printHelper decks
        putStrLn s
        prettyPrintTableRows newDecks
    else putStrLn $ concat $ replicate 210 "-"

prettyPrintTable :: Table -> IO ()
prettyPrintTable t@(decks, cell) = do
    prettyPrintCell cell
    prettyPrintTableRows $ map reverse decks

prettyPrintTables :: [Table] -> IO ()
prettyPrintTables [] = return ()
prettyPrintTables (t : ts) = do
    prettyPrintTable t
    prettyPrintTables ts

---------------------------SOLVE-------------------------------------

--            bottom   top
isValidStep :: Card -> Card -> Bool
isValidStep (Number n1 s1) (Number n2 s2) = n1 + 1 == n2 && suitToBool s1 /= suitToBool s2
isValidStep (Face _ s1) (Face _ s2) = s1 == s2
isValidStep _ _ = False

getMovableNumber :: Deck -> (Deck, Deck)
getMovableNumber [] = ([], [])
getMovableNumber [card] = ([card], [])
getMovableNumber (card1@(Number n1 s1) : card2@(Number n2 s2) : deck)
    | n1 + 1 == n2 && suitToBool s1 /= suitToBool s2 = ((card1 : all), (card1 : sub))
    | otherwise = ([card1], []) where
        (all, sub) = getMovableNumber (card2 : deck)
getMovableNumber (card : _) = ([card], [])

getMovableFace :: Deck -> (Deck, Deck)
getMovableFace [] = ([], [])
getMovableFace [card] = ([card], [])
getMovableFace (card1@(Face _ s1) : card2@(Face _ s2) : deck)
    | s1 == s2 = ((card1 : all), (card1 : sub))
    | otherwise = ([card1], []) where
        (all, sub) = getMovableFace (card2 : deck)
getMovableFace (card : _) = ([card], [])

--                     all    -1
getMovable :: Deck -> (Deck, Deck)
getMovable deck@((Number _ _) : _) = getMovableNumber deck
getMovable deck@((Face _ _) : _) = getMovableFace deck

tryMoveDeckToDeck :: Deck -> Deck -> (Deck, Deck)
tryMoveDeckToDeck startDeck [] = getMovable startDeck
tryMoveDeckToDeck startDeck destDeck
    | isSolvedDeck destDeck = (destDeck, [])
    | subMovableDeck /= [] && isValidStep (last' allMovableDeck) (head' destDeck) && isValidStep (last' subMovableDeck) (head' destDeck) = (allMovableDeck ++ destDeck, subMovableDeck ++ destDeck)
    | isValidStep (last' allMovableDeck) (head' destDeck) = (allMovableDeck ++ destDeck, [])
    | subMovableDeck /= [] && isValidStep (last' subMovableDeck) (head' destDeck) = (subMovableDeck ++ destDeck, [])
    | otherwise = (destDeck, []) where
        (allMovableDeck, subMovableDeck) = getMovable startDeck

--changesAfterMoving :: Deck -> Deck -> Bool
--changesAfterMoving startDeck destDeck = tryMoveDeckToDeck startDeck destDeck /= (destDeck, [])

step :: Deck -> [Deck] -> [Deck] -> [[Deck]]
step _ _ [] = []
step [] _ _ = []
step deck processedDecks (nextDeck : decks)
    | isSolvedDeck deck = []
    | allNewDeck /= nextDeck && subNewDeck /= [] = ((reverse processedDecks) ++ allNewDeck : decks) : ((reverse processedDecks) ++ subNewDeck : decks) : (step deck (nextDeck : processedDecks) decks) 
    | allNewDeck /= nextDeck = ((reverse processedDecks) ++ allNewDeck : decks) : (step deck (nextDeck : processedDecks) decks) 
    | otherwise = (step deck (nextDeck : processedDecks) decks) where
        (allNewDeck, subNewDeck) = tryMoveDeckToDeck deck nextDeck

remaningDeckAfterMoving :: Deck -> (Deck, Deck)
remaningDeckAfterMoving deck = (drop (length allMovableDeck) deck, drop (length subMovableDeck) deck) where
    (allMovableDeck, subMovableDeck) = getMovable deck

getTruncatedDecks :: [Deck] -> [[Deck]]
getTruncatedDecks decks = do
    n <- [0.. length decks - 1]
    if (not $ null $ decks !! n) then return $ take n decks ++ (tail' (decks !! n)) : (drop (n + 1) decks)
                                 else return []

stepsExtra :: Table -> [Table]
stepsExtra (decks, (Just card)) = zip (step [card] [] decks) $ repeat Nothing
stepsExtra (decks, Nothing) = filter (\case {(_, Just _) -> True; _ -> False}) $ zip (getTruncatedDecks decks) (map (\deck -> if null deck || isSolvedDeck deck then Nothing else Just $ head' deck) decks)

stepsDecks :: Table -> [Table]
stepsDecks (decks, extra) = do
    n <- [0..8]
    let processedDecks = take n decks
    let currentDeck = decks !! n
    let remainingDecks = drop (n + 1) decks
    let newDeckss = step currentDeck [] (processedDecks ++ remainingDecks)
    let (allRemaining, subRemaining) = remaningDeckAfterMoving currentDeck
    case extra of
        Just card -> do
            let missingDecks = map (\decks -> if validateDecks ([card] : allRemaining : decks) then allRemaining else subRemaining) newDeckss
            zip (zipWith3 (\begin missing end -> begin ++ missing : end) (map (take n) newDeckss) missingDecks (map (drop n) newDeckss)) $ repeat extra
        Nothing -> do 
            let missingDecks = map (\decks -> if validateDecks (allRemaining : decks) then allRemaining else subRemaining) newDeckss
            zip (zipWith3 (\begin missing end -> begin ++ missing : end) (map (take n) newDeckss) missingDecks (map (drop n) newDeckss)) $ repeat Nothing

steps :: Table -> [Table]
steps table = stepsExtra table ++ stepsDecks table

iterateTables :: [Table] -> [Table] -> ([Table], [Table])
iterateTables knownTables currentTables = (union knownTables newTables, newTables \\ knownTables) where
    newTables = concat $ map steps currentTables

solve :: [Table] -> [Table] -> Table
solve _ [] = ([], Nothing)
solve knownTables currentTables
    | any isSolvedTable currentTables = fromJust $ find isSolvedTable currentTables
    | otherwise = solve newKnownTables newCurrentTables where
        (newKnownTables, newCurrentTables) = iterateTables knownTables currentTables

solve' :: [Table] -> [Table] -> Int -> [Table]
solve' _ currentTables 0 = currentTables
solve' knownTables currentTables n = solve' newKnownTables newCurrentTables (n - 1) where
    (newKnownTables, newCurrentTables) = iterateTables knownTables currentTables

solution :: Table -> Table
solution table = solve [table] [table] where

base = prettyPrintTable testTable1
test = prettyPrintTables $ solve' [testTable2] [testTable2] 10