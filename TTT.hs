
-- Data types

data Square = Blank | Zero | Cross
  deriving (Eq)

type Row = [Square]
type Board = [Row]

-- Functions for creating rows and boards

createRow :: Row
createRow = [Zero, Zero, Zero]

createBoard :: Board
createBoard = [createRow, createRow, createRow]

-- Functions for printing a Square / Row / Board

printSquare :: Square -> IO ()
printSquare Blank = putStr " "
printSquare Zero =  putStr "0"
printSquare Cross = putStr "x"

printRow :: Row -> IO ()
printRow (x:[]) = printSquare x >> putStr "\n"
printRow (x:xs) = printSquare x >> putStr " " >> printRow xs

printBoard :: Board -> IO ()
printBoard (x:[]) = printRow x
printBoard (x:xs) = printRow x >> printBoard xs

-- Functions for updating a game

-- put :: Board -> Board
