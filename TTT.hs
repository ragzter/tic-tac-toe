
import Data.Char
import System.IO
import Data.List

-- Data types

data Square = Blank | Zero | Cross
  deriving (Eq)

instance Show Square where
  show Cross = "x"
  show Zero = "0"
  show Blank = "_"

data CSquare = CSquare Square Int Int
  deriving (Eq)

instance Show CSquare where
  show (CSquare s x y) = "(" ++ show s ++ " at " ++ show x ++ ", " ++ show y ++ ")"

isBlank :: CSquare -> Bool
isBlank (CSquare Blank _ _) = True
isBlank _ = False

coordinateOf :: CSquare -> (Int, Int)
coordinateOf (CSquare _ x y) = (x, y)

opponentOf :: Square -> Square
opponentOf Zero = Cross
opponentOf Cross = Zero
opponentOf x = x

sqToStr :: Square -> String
sqToStr Blank = " "
sqToStr Zero = "0"
sqToStr Cross = "x"

type Row = [Square]
type Line = [CSquare]
type Board = [Row]

-- Functions for creating rows and boards

createRow :: Row
createRow = [Blank, Blank, Blank]

createBoard :: Board
createBoard = [createRow, createRow, createRow]

-- Functions for printing a Square / Row / Board

printSquare :: Square -> IO ()
printSquare Blank = putStr " "
printSquare Zero = putStr "0"
printSquare Cross = putStr "x"

printRow :: Row -> IO ()
printRow (x:[]) = printSquare x >> putStr "\n"
printRow (x:xs) = printSquare x >> putStr "|" >> printRow xs

printBoard :: Board -> IO ()
printBoard (x:[]) = putStr " " >> printRow x
printBoard (x:xs) = putStr " " >> printRow x >> putStr " -----\n" >> printBoard xs

prettyPrintBoard :: Board -> IO ()
prettyPrintBoard b = putStr "\n" >> printBoard b >> putStr "\n"

-- Functions for updating a game

replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) 0 r = r : xs
replaceNth (x:xs) n r = x : replaceNth xs (n - 1) r

put :: Board -> Int -> Int -> Square -> Board
put b x y s = replaceNth b y $ replaceNth (b !! y) x s

readMove :: Square -> Board -> IO Board
readMove s b = do {
  ; sStr <- return $ sqToStr s
  ; putStr $ "Move (" ++ sStr ++ "): "
  ; hFlush stdout
  ; move <- getLine
  ; xc <- return $ move !! 0
  ; yc <- return $ move !! 2
  ; x <- return $ digitToInt xc
  ; y <- return $ digitToInt yc
  ; if squareAt b x y /= Blank then
      return b
    else
      return $ put b x y s
  }

-- Functions for determining a winner

squareAt :: Board -> Int -> Int -> Square
squareAt b x y = (b !! y) !! x

sameAndNotBlank :: Square -> Square -> Square -> Maybe Square
sameAndNotBlank a b c = if a /= Blank && a == b && b == c then Just a else Nothing

rowWinner :: Board -> Int -> Maybe Square
rowWinner b n = sameAndNotBlank (squareAt b 0 n) (squareAt b 1 n) (squareAt b 2 n)

notNothing :: [Maybe Square] -> Maybe Square
notNothing [] = Nothing
notNothing (x:xs) = if x /= Nothing then x else notNothing xs

multiRowWinner :: Board -> Maybe Square
multiRowWinner b = notNothing [rowWinner b 0, rowWinner b 1, rowWinner b 2]

columnWinner :: Board -> Int -> Maybe Square
columnWinner b n = sameAndNotBlank (squareAt b n 0) (squareAt b n 1) (squareAt b n 2)

multiColumnWinner :: Board -> Maybe Square
multiColumnWinner b = notNothing [columnWinner b 0, columnWinner b 1, columnWinner b 2]

leftDiagonalWinner :: Board -> Maybe Square
leftDiagonalWinner b = sameAndNotBlank (squareAt b 0 0) (squareAt b 1 1) (squareAt b 2 2)

rightDiagonalWinner :: Board -> Maybe Square
rightDiagonalWinner b = sameAndNotBlank (squareAt b 2 0) (squareAt b 1 1) (squareAt b 0 2)

diagonalWinner :: Board -> Maybe Square
diagonalWinner b = notNothing [leftDiagonalWinner b, rightDiagonalWinner b]

winnerOf :: Board -> Maybe Square
winnerOf b = notNothing [multiRowWinner b, multiColumnWinner b, diagonalWinner b]

draw :: Board -> Bool
draw b = (length $ filter ((==) Blank) $ concat b) == 0

-- The Game

maybeSqToIOSq :: Maybe Square -> IO Square
maybeSqToIOSq Nothing = return Blank
maybeSqToIOSq (Just x) = return x

announceWinner :: Square -> IO ()
announceWinner s = do {
  ; str <- return $ sqToStr s
  ; putStr $ "\n" ++ str ++ " won the game, congratulations!\n\n"
  }

detectAndHandleTheft :: Board -> Board -> Square -> IO ()
detectAndHandleTheft b nb s =
  if nb == b then
    putStr "You can't steal another persons square!  Redo!\n\n" >> loopGame s b
  else
    return ()

loopGame :: Square -> Board -> IO ()
loopGame s b = do {
  ; prettyPrintBoard b
  ; nb <-
    if s == Zero then
      return $ computerMove b
    else
      readMove s b
  ; detectAndHandleTheft b nb s
  ; winner <- maybeSqToIOSq $ winnerOf nb
  ; if winner /= Blank then
      prettyPrintBoard nb >> announceWinner winner
    else
      if draw nb then
        prettyPrintBoard nb >> putStr "\nEveryone is a winner!\n\n"
      else
        loopGame (opponentOf s) nb
  }

main :: IO ()
main = do {
  ; putStr "\nWelcome to TTT!  'x' starts -- go!\n\n"
  ; board <- return createBoard
  ; loopGame Cross board
  }

-- In progress

testBoard :: Board
testBoard = [[Cross, Blank, Cross], [Zero, Cross, Blank], [Blank, Blank, Cross]]

testLine :: Line
testLine = head $ getLines testBoard

sqsToLine :: [Square] -> String -> Int -> Line
sqsToLine sqs "row" n =
  [
    CSquare (sqs !! 0) 0 n,
    CSquare (sqs !! 1) 1 n,
    CSquare (sqs !! 2) 2 n
  ]
sqsToLine sqs "column" n =
  [
    CSquare (sqs !! 0) n 0,
    CSquare (sqs !! 1) n 1,
    CSquare (sqs !! 2) n 2
  ]
sqsToLine sqs "diagonal" 0 =
  [
    CSquare (sqs !! 0) 0 0,
    CSquare (sqs !! 1) 1 1,
    CSquare (sqs !! 2) 2 2
  ]
sqsToLine sqs "diagonal" 1 =
  [
    CSquare (sqs !! 0) 2 0,
    CSquare (sqs !! 1) 1 1,
    CSquare (sqs !! 2) 0 2
  ]

getLines :: Board -> [Line]
getLines b =
  [
    sqsToLine (b !! 0) "row" 0,
    sqsToLine (b !! 1) "row" 1,
    sqsToLine (b !! 2) "row" 2,
    sqsToLine (transpose b !! 0) "column" 0,
    sqsToLine (transpose b !! 1) "column" 1,
    sqsToLine (transpose b !! 2) "column" 2,
    sqsToLine [squareAt b 0 0, squareAt b 1 1, squareAt b 2 2] "diagonal" 0,
    sqsToLine [squareAt b 2 0, squareAt b 1 1, squareAt b 0 2] "diagonal" 1
  ]

nOf :: Eq a => a -> [a] -> Int
nOf x xs = length $ filter (\y -> x == y) xs

exists :: Eq a => a -> [a] -> Bool
exists x xs = (find (\y -> x == y) xs) /= Nothing

csqToSq :: CSquare -> Square
csqToSq (CSquare s _ _) = s

opportunity :: Line -> Bool
opportunity l =
  nOf Zero sqs == 2 &&
  exists Blank sqs
  where
    sqs = map csqToSq l

risky :: Line -> Bool
risky l =
  nOf Cross sqs == 2 &&
  exists Blank sqs
  where
    sqs = map csqToSq l

selfPopulated :: Line -> Bool
selfPopulated l =
  exists Zero sqs
  where
    sqs = map csqToSq l

coordinateOfBlank :: Line -> (Int, Int)
coordinateOfBlank l =
  coordinateOf $ head $ filter isBlank l

applyLine :: Line -> Board -> Board
applyLine l b =
  put b x y Zero
  where
    coordinates = coordinateOfBlank l
    x = fst coordinates
    y = snd coordinates

computerMove :: Board -> Board
computerMove b =
  if length opportunities > 0 then
    applyLine (opportunities !! 0) b
  else if length risks > 0 then
    applyLine (risks !! 0) b
  else if length selfPopulatedLines > 0 then
    applyLine (selfPopulatedLines !! 0) b
  else
    applyLine (lines !! 0) b
  where
    lines = getLines b
    opportunities = filter opportunity lines
    risks = filter risky lines
    selfPopulatedLines = filter selfPopulated lines
    line = head lines
