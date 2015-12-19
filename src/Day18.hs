{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Day18 where

import           Data.Array.Unboxed

type Pos = (Int, Int)
type Value = (Pos, Bool)

tick :: Array Pos ((Int, Int), Bool) -> Array Pos ((Int, Int), Bool)
tick x = amap (advance x) x

tick' :: Array Pos Value -> Array Pos Value
tick' = lightCorners . tick

advance :: Array Pos (a1, Bool) -> ((Int, Int), Bool) -> ((Int, Int), Bool)
advance x (i, v)
  | length (neighbors x i) == 3 = (i, True)
  | v && length (neighbors x i) == 2 = (i, True)
  | otherwise                  = (i, False)

neighbors :: Array Pos (a1, Bool) -> (Int, Int) -> [Pos]
neighbors x (i, j) = filter (\d -> snd $ x ! d) $ filter (inBounds b) [
    (i - 1, j - 1), (i, j - 1), (i + 1, j - 1),
    (i - 1, j),                 (i + 1, j),
    (i - 1, j + 1), (i, j + 1), (i + 1, j + 1)
  ]
  where
    b = bounds x

inBounds :: (Pos, Pos) -> Pos -> Bool
inBounds ((bnx, bny), (bxx, bxy)) (i, j) = i >= bnx && j >= bny && i <= bxx && j <= bxy

on :: Char -> Bool
on '#' = True
on _ = False

count :: Array Pos Value -> Int
count = length . filter id . map snd . elems

lightCorners :: Array Pos Value -> Array Pos Value
lightCorners x = (x //) $ map (\x -> (x, (x, True))) corners
  where
    ((nx, ny), (xx, xy)) = bounds x
    corners = [(nx, ny), (nx, xy), (xx, ny), (xx, xy)]

--------------------------------------------------------------------------------

parse :: IArray a Value => [String] -> a Pos Value
parse xs = array (fst . head $ es, fst . last $ es) es
  where
    es = concat . addIndex . map (map on) $ xs

addIndex :: [[Bool]] -> [[(Pos, Value)]]
addIndex = map (\(x, xs) -> map (\(y, v) -> ((x, y), ((x, y), v))) $ zip [1..] xs) . zip [1..]

