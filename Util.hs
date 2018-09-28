
module Util where

import Square
import Data.List

replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ _ = []
replaceNth (_:xs) 0 r = r : xs
replaceNth (x:xs) n r = x : replaceNth xs (n - 1) r

nOf :: Eq a => a -> [a] -> Int
nOf x xs = length $ filter (\y -> x == y) xs

exists :: Eq a => a -> [a] -> Bool
exists x xs = (find (\y -> x == y) xs) /= Nothing

maybeSqToIOSq :: Maybe Square -> IO Square
maybeSqToIOSq Nothing = return Blank
maybeSqToIOSq (Just x) = return x
