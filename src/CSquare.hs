
module CSquare where

import Square

data CSquare = CSquare { square :: Square, x :: Int, y :: Int }
  deriving (Eq)

instance Show CSquare where
  show (CSquare s x y) = "(" ++ show s ++ " at " ++ show x ++ ", " ++ show y ++ ")"

isBlank :: CSquare -> Bool
isBlank (CSquare Blank _ _) = True
isBlank _ = False

coordinateOf :: CSquare -> (Int, Int)
coordinateOf (CSquare _ x y) = (x, y)

csqToSq :: CSquare -> Square
csqToSq (CSquare s _ _) = s
