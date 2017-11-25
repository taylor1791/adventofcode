{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day13 where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

import Data.List (permutations, maximumBy)

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

addApetite :: String -> [((String, String), Int)] -> [((String, String), Int)]
addApetite n xs = (xs ++) $ map (\x -> (x, 0)) $ flips $ map (\p -> (n, p)) $ people xs

flips :: [(a, a)] -> [(a, a)]
flips [] = []
flips ((x, y):xs) = (x, y) : (y, x) : flips xs

optimize :: [((String, String), Int)] -> ([String], Int)
optimize xs = maximumBy compare' [(seating, value xs seating) | seating <- seatings $ people xs]
  where
    compare' y z = compare (snd y) (snd z)

people :: [((String, String), Int)] -> [String]
people (x:xs) = first x : (map first $ filter (\((_, s2), _) -> s2 == first x) xs)
  where
    first = fst . fst

value :: [((String, String), Int)] -> [String] -> Int
value table xs = maybe undefined id $ fmap sum $ sequence $ map (\y -> lookup' table y) $ zip xs $ drop 1 xs

lookup' :: [((String, String), Int)] -> (String, String) -> Maybe Int
lookup' table (x, y) = do
  x' <- lookup (x, y) table
  y' <- lookup (y, x) table
  return $ x' + y'

seatings :: [String] -> [[String]]
seatings (x:xs) = map (\ys -> x : ys ++ [x]) $ permutations xs

parse :: String -> Either P.ParseError ((String, String), Int)
parse = P.parse parseHappiness "(source)"

parseHappiness :: Parsec s u m ((String, String), Int)
parseHappiness = do
  n1 <- parseIdentifier
  P.string " would "
  f <- parseSign
  PC.char ' '
  v <- parseInt
  P.string " happiness units by sitting next to "
  n2 <- parseIdentifier
  return ((n1, n2), f v)

parseIdentifier :: Parsec s u m String
parseIdentifier = P.many PC.letter

parseGain :: Parsec s u m (Int -> Int)
parseGain = PC.string "gain" >> return ((1 :: Int) *)

parseLose :: Parsec s u m (Int -> Int)
parseLose = PC.string "lose" >> return ((-1 :: Int) *)

parseInt :: Parsec s u m Int
parseInt = read <$> P.many1 P.digit

parseSign :: Parsec s u m (Int -> Int)
parseSign = P.try parseGain P.<|> P.try parseLose

