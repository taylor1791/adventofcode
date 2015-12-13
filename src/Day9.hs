{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Day9 where

import Data.List (nub, permutations, minimumBy)

import qualified Text.Parsec as P
import Text.Parsec.Char (string)

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

makeSymmetric :: [((String, String), Int)] -> [((String, String), Int)]
makeSymmetric [] = []
makeSymmetric ((x, y):xs) = (x, y):(flipPair x, y): makeSymmetric xs

flipPair :: (a, b) -> (b, a)
flipPair (x, y) = (y, x)

tsp' :: [((String, String), Int)] -> ([String], Int)
tsp' = negate2nd . tsp . map negate2nd

negate2nd :: (a, Int) -> (a, Int)
negate2nd (x, y) = (x, -y)

-- Looks like brute force is good enough! Which is a good thing
-- beause I don't remember branch and bound very well.
tsp :: [((String, String), Int)] -> ([String], Int)
tsp distances = (solution, pathCost solution distances)
  where
    cities = nub $ map (\((a, _), _) -> a) distances
    ts = tours cities
    compareTour t1 t2 = compare (pathCost t1 distances) $ pathCost t2 distances
    solution = minimumBy compareTour ts

tours :: [String] -> [[String]]
tours = permutations

pathCost :: [String] -> [((String, String), Int)] -> Int
pathCost xs costs = sum $ map (cost costs) moves
  where
    moves = pairs xs

cost :: [((String, String), Int)] -> (String, String) -> Int
cost costs move = case lookup move costs of
  Nothing -> error $ "Cannot find " ++ show move ++ " in costs map"
  Just x -> x

pairs :: [String] -> [(String, String)]
pairs (x:[]) = [(x, x)]
pairs zs = zip zs $ drop 1 zs

parse :: String -> ((String, String), Int)
parse x = case P.parse parseLine "(source)" x of
  Left _ -> error $ "Failed to parse " ++ show x
  Right e -> e

parseLine :: Parsec s u m ((String, String), Int)
parseLine = do
  c1 <- parseCity
  string " to "
  c2 <- parseCity
  string " = "
  x <- parseInt
  return ((c1, c2), x)

parseCity :: Parsec s u m String
parseCity = P.many1 P.letter

parseInt :: Parsec s u m Int
parseInt = read <$> P.many1 P.digit

