module Day20 where

import           Data.List (nub)

presents' :: Int -> Int
presents' n = (11 *) . sum . filter (\x -> n <= 50 * x) . factors $ n

presents :: Int -> Int
presents = (10 *) . sum . factors

factors :: Int -> [Int]
factors n = nub . concat $ [[x, y] | x <- [1..isqrt n], let (y, q) = n `divMod` x, q == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral'
  where
    fromIntegral' :: Int -> Double
    fromIntegral' = fromIntegral

