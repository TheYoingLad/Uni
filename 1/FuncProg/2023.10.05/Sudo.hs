module Sudo where

type Pos = (Int, Int)
type Cell = (Pos, Int)
type Sudoku = [Cell]
type Block = Int

sudoku :: Sudoku
sudoku = [((0,0),3),((0,1),6),((0,4),7),((0,5),1),((0,6),2),
          ((1,1),5),((1,6),1),((1,7),8),
          ((2,2),9),((2,3),2),((2,5),4),((2,6),7),
          ((3,4),1),((3,5),3),((3,7),2),((3,8),8),
          ((4,0),4),((4,3),5),((4,5),2),((4,8),9),
          ((5,0),2),((5,1),7),((5,3),4),((5,4),6),
          ((6,2),5),((6,3),3),((6,5),8),((6,6),9),
          ((7,1),8),((7,2),3),((7,7),6),
          ((8,2),7),((8,3),6),((8,4),9),((8,7),4),((8,8),3)]

sudoku2 :: Sudoku
sudoku2 = [((0,0),5),((0,1),3),((0,4),7),
           ((1,0),6),((1,3),1),((1,4),9),((1,5),5),
           ((2,1),9),((2,2),8),((2,7),6),
           ((3,0),8),((3,4),6),((3,8),3),
           ((4,0),4),((4,3),8),((4,5),3),((4,8),1),
           ((5,0),7),((5,4),2),((5,8),6),
           ((6,1),6),((6,6),2),((6,7),8),
           ((7,3),4),((7,4),1),((7,5),9),((7,8),5),
           ((8,4),8),((8,7),7),((8,8),9)]

numsInRow :: Sudoku -> Int -> [Int]
--numsInRow x y = [snd(x!!n) | n <-[0..((length x)-1)], fst(fst(x!!n))==y]
numsInRow x y = [c | ((a, b), c)<-x, y == a]

numsInCol :: Sudoku -> Int -> [Int]
--numsInCol x y = [snd(x!!n) | n <-[0..((length x)-1)], snd(fst(x!!n))==y]
numsInCol x y = [c | ((a, b), c)<-x, y == b]

posToBlock :: Pos -> Block
posToBlock (x, y) = x - (mod x 3) + div y 3

blockToPositions :: Block -> [Pos]
blockToPositions x 
    | x < 0 || x > 8 = error "bad block"
    | otherwise = [(n,m) | n <-[(x-(mod x 3))..((x-(mod x 3))+2)], m <-[((mod x 3)*3)..(((mod x 3)*3)+2)]]

numsInBlock :: Sudoku -> Block -> [Int]
--numsInBlock x y = [(snd(unzip x))!!i | i<-[0..((length x)-1)], m <-(blockToPositions y), m==fst(x!!i)]
numsInBlock x y = [b | c <- (blockToPositions y), (a, b) <- x, a == c]

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique [a] = True
allUnique (a:b) = not(elem a b) && allUnique(b)

isSudokuPuzzle :: Sudoku -> Bool
isSudokuPuzzle x = null[1 | ((a, b), c) <- x, not (elem a [0..8]) || not(elem b[0..8]) || not(elem c [1..9])] &&
                   null[n | n<-[0..8], not (allUnique(numsInRow x n)) || not (allUnique(numsInCol x n)) || not (allUnique(numsInBlock x n))]

isFilled :: Sudoku -> Bool
isFilled x = length x == 81 && allUnique (fst(unzip x))

isSolved :: Sudoku -> Bool
isSolved x = isFilled x && isSudokuPuzzle x

isBlank :: Sudoku -> Pos -> Bool
isBlank x y = not (elem y [a | (a, b) <- x])

blankPositions :: Sudoku -> [Pos]
blankPositions x = [n | n<-[(j,k) | j<-[0..8], k<-[0..8]], isBlank x n]

possibleNumsOnPos :: Sudoku -> Pos -> [Int]
possibleNumsOnPos x y
    | not(isBlank x y) = []
    | otherwise = [n | n<-[1..9], not (elem n (numsInRow x (fst y))), not (elem n (numsInCol x (snd y))), not (elem n (numsInBlock x (posToBlock y)))]


possibleNumsForBlankPos :: Sudoku -> [(Pos, [Int])]
possibleNumsForBlankPos x = [(n,m) | n<-[(j,k) | j<-[0..8], k<-[0..8]], m<-[possibleNumsOnPos x n], isBlank x n, possibleNumsOnPos x n == m]

hasSolution :: [(Pos, [Int])] -> Bool
hasSolution x
    | x == [] = False
    | elem [] (snd(unzip x)) = False
    | otherwise = True

uniqueNumForBlankPos :: [(Pos, [Int])] -> [(Pos, Int)]
uniqueNumForBlankPos x = [(n,m) | i<-[0..((length x)-1)], n<-(fst(unzip x)), m<-[1..9], length(snd(x!!i)) == 1, fst(x!!i) == n, head(snd(x!!i)) == m]

insertElem :: Sudoku -> Pos -> Int -> Sudoku
insertElem x y z
    | not (isBlank x y) = error "position is not blank"
    | otherwise = (y,z):x

step :: Sudoku -> [Sudoku]
step x
    | isSolved x = [x]
    | not (hasSolution (possibleNumsForBlankPos x)) = []
    | not (null (uniqueNumForBlankPos((possibleNumsForBlankPos x)))) = [insertElem x (fst(head(uniqueNumForBlankPos((possibleNumsForBlankPos x))))) (snd(head(uniqueNumForBlankPos((possibleNumsForBlankPos x)))))]
    | otherwise = [n | m<-[0..((length(snd(head(possibleNumsForBlankPos x))))-1)], n<-[insertElem x (fst(head(possibleNumsForBlankPos x))) ((snd(head(possibleNumsForBlankPos x)))!!m)]]

solve :: Sudoku -> [Sudoku]
solve x
    | not(isSudokuPuzzle (x)) = error "improper sudoku"
    | step x == [x] = [x]
    | otherwise = [n | m<-[0..((length(step x))-1)], n<-solve((step x)!!m)]