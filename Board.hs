
module Board where

import Data.List

import Square
import Row
import Util

data Board = Board { rows :: [Row] }
  deriving (Eq)

instance Show Board where
  show (Board rs) = "\n" ++ (concat (intersperse " -----\n" (map show rs))) ++ "\n"

createBoard :: Board
createBoard = Board [createRow, createRow, createRow]

sampleBoard :: Board
sampleBoard =
  Board [Row [Cross, Zero, Cross],
         Row [Blank, Cross, Blank],
         Row [Blank, Blank, Cross]]

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
