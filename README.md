# tic-tac-toe
Tic Tac Toe written in Haskell

Includes computer opponent!

Just for fun!

## How to play

`ghc TTT.hs -o ttt && ./ttt`

## Example game

```
~/code/ttt $ ghc TTT.hs -o ttt && ./ttt

Welcome to TTT!  'x' starts -- go!


  | | 
 -----
  | | 
 -----
  | | 

Move (x): 0 0

 x| | 
 -----
  | | 
 -----
  | | 


 x|0| 
 -----
  | | 
 -----
  | | 

Move (x): 2 0

 x|0|x
 -----
  | | 
 -----
  | | 


 x|0|x
 -----
  |0| 
 -----
  | | 

Move (x): 1 2

 x|0|x
 -----
  |0| 
 -----
  |x| 


 x|0|x
 -----
 0|0| 
 -----
  |x| 

Move (x): 2 1

 x|0|x
 -----
 0|0|x
 -----
  |x| 


 x|0|x
 -----
 0|0|x
 -----
  |x|0

Move (x): 0 2

 x|0|x
 -----
 0|0|x
 -----
 x|x|0


Everyone is a winner!

~/code/ttt $ 
```
