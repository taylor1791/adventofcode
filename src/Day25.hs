module Day25 where

cantor :: (Int, Int) -> Int
cantor (a, b) = (a + b) * (a + b + 1) `div` 2 + b

snowMachine :: (Int, Int) -> Int
snowMachine (a, b) = head $ drop (cantor (a - 1, b - 1)) $ iterate snow 20151125

snow :: Int -> Int
snow x = x * 252533 `mod` 33554393


