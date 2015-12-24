module Day24 where

import           Data.Function (on)
import           Data.List     (minimumBy, tails)

combinations :: [a] -> Int -> [[a]]
combinations _ 0  = [ [] ]
combinations xs n = [ y:ys | y:xs' <- tails xs, ys <- combinations xs' (n-1)]

compartment1 comps packages =
  minimumBy compare' $ filter correctWeight $ concatMap (combinations packages) [1..6]
  where
    compare' x y = (compare `on` length) x y `mappend` (compare `on` product) x y
    correctWeight = (== sum packages `div` comps) . sum

