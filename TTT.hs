
import Data.Char
import System.IO
import Data.List

-- Data types

data Square = Blank | Zero | Cross
  deriving (Eq)

instance Show Square where
  show Cross = "x"
  show Zero = "0"
  show Blank = " "

data CSquare = CSquare { square :: Square, x :: Int, y :: Int }
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

data Row = Row { squares :: [Square] }
  deriving (Eq)

instance Show Row where
  show (Row sqs) = " " ++ (concat (intersperse "|" (map show sqs))) ++ "\n"

data Line = Line { cSquares :: [CSquare] }

instance Show Line where
  show (Line csqs) = concat (intersperse "--" (map show csqs))

data Board = Board { rows :: [Row] }
  deriving (Eq)

instance Show Board where
  show (Board rs) = "\n" ++ (concat (intersperse " -----\n" (map show rs))) ++ "\n"

-- Functions for creating rows and boards

createRow :: Row
createRow = Row [Blank, Blank, Blank]

createBoard :: Board
createBoard = Board [createRow, createRow, createRow]

testRow :: Row
testRow = Row [Cross, Blank, Zero]

testBoard :: Board
testBoard =
  Board [Row [Cross, Zero, Cross],
         Row [Blank, Cross, Blank],
         Row [Blank, Blank, Cross]]

replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) 0 r = r : xs
replaceNth (x:xs) n r = x : replaceNth xs (n - 1) r

replaceInRow :: Row -> Int -> Square -> Row
replaceInRow (Row sqs) x s = Row $ replaceNth sqs x s

put :: Board -> Int -> Int -> Square -> Board
put (Board rs) x y s =
  let newRow = replaceInRow (rs !! y) x s
      newBoard = replaceNth rs y newRow
  in
    Board newBoard

squareAt :: Board -> Int -> Int -> Square
squareAt (Board rs) x y =
  let (Row sqs) = rs !! y
  in
    sqs !! x

-- Functions for updating a game

readMove :: Square -> Board -> IO Board
readMove s b =
  do {
  ; putStr $ "Move (" ++ show s ++ "): "
  ; hFlush stdout
  ; move <- getLine
  ; x <- return $ digitToInt $ move !! 0
  ; y <- return $ digitToInt $ move !! 2
  ; if squareAt b x y /= Blank then
      return b
    else
      return $ put b x y s
  }

-- Functions for determining a winner

winnerOf :: Board -> Maybe Square
winnerOf b =
  let lines = getLines b
      checkLineWinner (Line cSqs) =
        square (cSqs !! 0) == square (cSqs !! 1) &&
        square (cSqs !! 1) == square (cSqs !! 2) &&
        square (cSqs !! 0) /= Blank
      lineWinner = filter checkLineWinner lines
  in
    if length lineWinner > 0 then
      Just (square $ head $ cSquares $ head lineWinner)
    else
      Nothing

draw :: Board -> Bool
draw (Board rs) =
  let occupiedRowP (Row sqs) = sqs !! 0 /= Blank && sqs !! 1 /= Blank && sqs !! 2 /= Blank
  in
    occupiedRowP (rs !! 0) && occupiedRowP (rs !! 1) && occupiedRowP (rs !! 2)

-- Line code

sqsToLine :: [Square] -> String -> Int -> Line
sqsToLine sqs "row" n = Line
  [
    CSquare (sqs !! 0) 0 n,
    CSquare (sqs !! 1) 1 n,
    CSquare (sqs !! 2) 2 n
  ]
sqsToLine sqs "column" n = Line
  [
    CSquare (sqs !! 0) n 0,
    CSquare (sqs !! 1) n 1,
    CSquare (sqs !! 2) n 2
  ]
sqsToLine sqs "diagonal" 0 = Line
  [
    CSquare (sqs !! 0) 0 0,
    CSquare (sqs !! 1) 1 1,
    CSquare (sqs !! 2) 2 2
  ]
sqsToLine sqs "diagonal" 1 = Line
  [
    CSquare (sqs !! 0) 2 0,
    CSquare (sqs !! 1) 1 1,
    CSquare (sqs !! 2) 0 2
  ]

getLines :: Board -> [Line]
getLines (Board rs) =
  let rowsAsSquares = map squares rs
  in
    [
      sqsToLine (squares (rs !! 0)) "row" 0,
      sqsToLine (squares (rs !! 1)) "row" 1,
      sqsToLine (squares (rs !! 2)) "row" 2,
      sqsToLine (transpose rowsAsSquares !! 0) "column" 0,
      sqsToLine (transpose rowsAsSquares !! 1) "column" 1,
      sqsToLine (transpose rowsAsSquares !! 2) "column" 2,
      sqsToLine [squareAt (Board rs) 0 0, squareAt (Board rs) 1 1, squareAt (Board rs) 2 2] "diagonal" 0,
      sqsToLine [squareAt (Board rs) 2 0, squareAt (Board rs) 1 1, squareAt (Board rs) 0 2] "diagonal" 1
    ]

testLine :: Line
testLine = head $ getLines testBoard

-- The Game

maybeSqToIOSq :: Maybe Square -> IO Square
maybeSqToIOSq Nothing = return Blank
maybeSqToIOSq (Just x) = return x

announceWinner :: Square -> IO ()
announceWinner s = do {
  ; putStr $ "\n" ++ show s ++ " won the game, congratulations!\n\n"
  }

detectAndHandleTheft :: Board -> Board -> Square -> IO ()
detectAndHandleTheft b nb s =
  if nb == b then
    putStr "You can't steal another persons square!  Redo!\n\n" >> loopGame s b
  else
    return ()

loopGame :: Square -> Board -> IO ()
loopGame s b = do {
  ; putStr $ show b
  ; nb <-
    if s == Zero then
      return $ computerMove b
    else
      readMove s b
  ; detectAndHandleTheft b nb s
  ; winner <- maybeSqToIOSq $ winnerOf nb
  ; if winner /= Blank then
      putStr (show nb) >> announceWinner winner
    else
      if draw nb then
        putStr $ (show nb) ++ "\nEveryone is a winner!\n\n"
      else
        loopGame (opponentOf s) nb
  }

main :: IO ()
main = do {
  ; putStr "\nWelcome to TTT!  'x' starts -- go!\n\n"
  ; board <- return createBoard
  ; loopGame Cross board
  }

-- Computer opponent

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
    sqs = map csqToSq $ cSquares l

risky :: Line -> Bool
risky l =
  nOf Cross sqs == 2 &&
  exists Blank sqs
  where
    sqs = map csqToSq $ cSquares l

selfPopulated :: Line -> Bool
selfPopulated l =
  exists Zero sqs &&
  exists Blank sqs
  where
    sqs = map csqToSq $ cSquares l

available :: Line -> Bool
available l =
  exists Blank sqs
  where
    sqs = map csqToSq $ cSquares l

coordinateOfBlank :: Line -> (Int, Int)
coordinateOfBlank l =
  coordinateOf $ head $ filter isBlank $ cSquares l

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
    applyLine availableLine b
  where
    lines = getLines b
    opportunities = filter opportunity lines
    risks = filter risky lines
    selfPopulatedLines = filter selfPopulated lines
    availableLine = head $ filter available lines
