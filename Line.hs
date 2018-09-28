
module Line where

import Data.List

import Square
import Row
import CSquare
import Board

data Line = Line { cSquares :: [CSquare] }

instance Show Line where
  show (Line csqs) = concat (intersperse "--" (map show csqs))

sampleLine :: Line
sampleLine = head $ getLines sampleBoard

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
sqsToLine _ _ _ = Line []

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
