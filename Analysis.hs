
module Analysis where

import Square
import CSquare
import Row
import Line
import Board

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

