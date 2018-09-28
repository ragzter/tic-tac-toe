
module Row where

import Data.List

import Square
import Util

data Row = Row { squares :: [Square] }
  deriving (Eq)

instance Show Row where
  show (Row sqs) = " " ++ (concat (intersperse "|" (map show sqs))) ++ "\n"

createRow :: Row
createRow = Row [Blank, Blank, Blank]

sampleRow :: Row
sampleRow = Row [Cross, Blank, Zero]

replaceInRow :: Row -> Int -> Square -> Row
replaceInRow (Row sqs) x s = Row $ replaceNth sqs x s
