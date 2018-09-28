
import Data.Char
import System.IO

import Square
import Board
import Util
import Analysis
import ComputerOpponent

main :: IO ()
main = do {
  ; putStr "\nWelcome to TTT!  'x' starts -- go!\n\n"
  ; board <- return createBoard
  ; loopGame Cross board
  }

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
