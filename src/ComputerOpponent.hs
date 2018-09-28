
module ComputerOpponent where

import Square
import CSquare
import Line
import Board
import Util

opportunity :: Line -> Square -> Bool
opportunity l s =
  nOf s sqs == 2 &&
  exists Blank sqs
  where
    sqs = map csqToSq $ cSquares l

risky :: Line -> Square -> Bool
risky l s =
  nOf (opponentOf s) sqs == 2 &&
  exists Blank sqs
  where
    sqs = map csqToSq $ cSquares l

selfPopulated :: Line -> Square -> Bool
selfPopulated l s =
  exists s sqs &&
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

applyLine :: Line -> Board -> Square -> Board
applyLine l b s =
  put b x y s
  where
    coordinates = coordinateOfBlank l
    x = fst coordinates
    y = snd coordinates

computerMove :: Board -> Square -> Board
computerMove b s =
  if length opportunities > 0 then
    applyLine (opportunities !! 0) b s
  else if length risks > 0 then
    applyLine (risks !! 0) b s
  else if length selfPopulatedLines > 0 then
    applyLine (selfPopulatedLines !! 0) b s
  else if squareAt b 1 1 == Blank then
    put b 1 1 s
  else
    applyLine availableLine b s
  where
    lines = getLines b
    opportunities = filter (\x -> opportunity x s) lines
    risks = filter (\x -> risky x s) lines
    selfPopulatedLines = filter (\x -> selfPopulated x s) lines
    availableLine = head $ filter available lines
