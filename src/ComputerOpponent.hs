
module ComputerOpponent where

import Square
import CSquare
import Line
import Board
import Util

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
  else if squareAt b 1 1 == Blank then
    put b 1 1 Zero
  else
    applyLine availableLine b
  where
    lines = getLines b
    opportunities = filter opportunity lines
    risks = filter risky lines
    selfPopulatedLines = filter selfPopulated lines
    availableLine = head $ filter available lines
