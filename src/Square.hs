
module Square where

data Square = Blank | Zero | Cross
  deriving (Eq)

instance Show Square where
  show Cross = "x"
  show Zero = "0"
  show Blank = " "

opponentOf :: Square -> Square
opponentOf Zero = Cross
opponentOf Cross = Zero
opponentOf x = x
