module Day1 where

import Data.List (foldl')

move :: Char -> Integer -> Integer
move '(' = succ
move ')' = pred
move x   = error $ "unknown move type" ++ [x]

floor :: String -> Integer
floor = foldl' (flip move) 0

basement :: String -> Integer
basement = fst . foldl' nextMove (-1, 0) . zip [1..]-- foldl' nextMove (-1, 0) pos
  
nextMove :: (Integer, Integer) -> (Integer, Char) -> (Integer, Integer)
nextMove acc@(_, level) (pos, nextMove)
  | level == -1 = acc
  | otherwise = (pos, move nextMove level)

