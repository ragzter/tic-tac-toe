
import Data.Char
import System.IO

-- Data types

data Square = Blank | Zero | Cross
  deriving (Eq)

opponentOf :: Square -> Square
opponentOf Zero = Cross
opponentOf Cross = Zero
opponentOf x = x

sToStr :: Square -> String
sToStr Blank = " "
sToStr Zero = "0"
sToStr Cross = "x"

type Row = [Square]
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
printBoard (x:[]) = printRow x
printBoard (x:xs) = printRow x >> putStr "------\n" >> printBoard xs

-- Functions for updating a game

replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) 0 r = r : xs
replaceNth (x:xs) n r = x : replaceNth xs (n - 1) r

put :: Board -> Int -> Int -> Square -> Board
put b x y s = replaceNth b y $ replaceNth (b !! y) x s

readMove :: Square -> Board -> IO Board
readMove s b = do {
  ; sStr <- return $ sToStr s
  ; putStr $ "Move (" ++ sStr ++ "): "
  ; hFlush stdout
  ; move <- getLine
  ; xc <- return $ move !! 0
  ; yc <- return $ move !! 2
  ; x <- return $ digitToInt xc
  ; y <- return $ digitToInt yc
  ; return $ put b x y s
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

-- notNothing ss = head $ filter (\x -> x /= Nothing) ss

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

winner :: Board -> Maybe Square
winner b = notNothing [multiRowWinner b, multiColumnWinner b, diagonalWinner b]

-- winner [[a, b, c], _, _] = sameAndNotBlank a b c
-- winner [_, [a, b, c], _] = sameAndNotBlank a b c
-- winner [_, _, [a, b, c]] = sameAndNotBlank a b c
-- winner [[d, _, _], [e, _, _], [f, _, _]] = sameAndNotBlank a b c

-- The Game

loopGame s b = do {
  ; putStr "\n"
  ; printBoard b
  ; putStr "\n"
  ; nb <- readMove s b
  ; loopGame (opponentOf s) nb
  }

main :: IO ()
main = do {
  ; putStr "\nWelcome to TTT !  You are 'x' and you start -- go !\n\n"
  ; board <- return createBoard
  ; loopGame Cross board
  }
