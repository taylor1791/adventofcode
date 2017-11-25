{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day17 where

import Data.List (tails, minimumBy)

import qualified Text.Parsec as P

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

possibilitiesMin :: Int -> [Int] -> [[Int]]
possibilitiesMin n sizes = map fst $ filter ((minContainers ==) . snd) correctCapacities
  where
    correctCapacities = map (\x -> (x, length x)) $ possibilities n sizes
    minContainers = snd $ minimumBy (\x y -> compare (snd x) (snd y)) correctCapacities

possibilities :: Int -> [Int] -> [[Int]]
possibilities n xs = filter ((n ==) . sum) choices
  where
    choices = foldr (\x acc -> acc ++ combinations x xs) [[]] [1..length xs]

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

--------------------------------------------------------------------------------

parse :: String -> Either P.ParseError Int
parse = P.parse parseInt "(string)"

parseInt :: Parsec s u m Int
parseInt = read <$> P.many1 P.digit

