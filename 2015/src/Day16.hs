{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day16 where

import Data.Maybe (isNothing)

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

data MFCSAM = MFCSAM {
    children :: Maybe Int
  , cats :: Maybe Int
  , samoyeds :: Maybe Int
  , pomeranians :: Maybe Int
  , akitas :: Maybe Int
  , vizslas :: Maybe Int
  , goldfish :: Maybe Int
  , trees :: Maybe Int
  , cars :: Maybe Int
  , perfumes :: Maybe Int
}

findSue' :: MFCSAM -> [(int, MFCSAM)] -> (int, MFCSAM)
findSue' goal = head . filter (\(_, x) -> (match' goal x))

match' :: MFCSAM -> MFCSAM -> Bool
match' goal x = foldl (&&) True $
  [children', cats', samoyeds', pomeranians', akitas', vizslas', goldfish', trees', cars', perfumes']
  where
    children' = possible children x goal
    cats' = isNothing (cats x) || cats goal < cats x
    samoyeds' = possible samoyeds x goal
    pomeranians' = isNothing (pomeranians x) || pomeranians goal > pomeranians x
    akitas' = possible akitas x goal
    vizslas' = possible vizslas x goal
    goldfish' = isNothing (goldfish x) || goldfish goal > goldfish x
    trees' = isNothing (trees x) || trees goal < trees x
    cars' = possible cars x goal
    perfumes' = possible perfumes x goal

findSue :: MFCSAM -> [(int, MFCSAM)] -> (int, MFCSAM)
findSue goal = head . filter (\(_, x) -> (match goal x))

possible :: (MFCSAM -> Maybe Int) -> MFCSAM -> MFCSAM -> Bool
possible f x y
  | f x == Nothing = True
  | f x == f y     = True
  | otherwise      = False

match :: MFCSAM -> MFCSAM -> Bool
match x y = foldl (&&) True $ map (\f -> possible f y x) $
  [children, cats, samoyeds, pomeranians, akitas, vizslas, goldfish, trees, cars, perfumes]

--------------------------------------------------------------------------------

defaultMfcsam :: MFCSAM
defaultMfcsam = MFCSAM Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parse :: String -> Either P.ParseError (Int, MFCSAM)
parse = P.parse parseMfcsam "(string)"

parseInt :: Parsec s u m Int
parseInt = read <$> P.many1 P.digit

parseIdentifier :: Parsec s u m String
parseIdentifier = P.many PC.letter

parsePair :: Parsec s u m (String, Int)
parsePair = do
  x <- parseIdentifier
  P.string ": "
  n <- parseInt
  return (x, n)

adjust :: MFCSAM -> (String, Int) -> MFCSAM
adjust x (s, v) = case s of
  "children" -> x { children = Just v }
  "cats" -> x { cats = Just v }
  "samoyeds" -> x { samoyeds = Just v }
  "pomeranians" -> x { pomeranians = Just v }
  "akitas" -> x { akitas = Just v }
  "vizslas" -> x { vizslas = Just v }
  "goldfish" -> x { goldfish = Just v }
  "trees" -> x { trees = Just v }
  "cars" -> x { cars = Just v }
  "perfumes" -> x { perfumes = Just v }

parseMfcsam :: Parsec s u m (Int, MFCSAM)
parseMfcsam = do
  P.string "Sue "
  i <- parseInt
  P.string ": "
  v1 <- parsePair
  P.string ", "
  v2 <- parsePair
  P.string ", "
  v3 <- parsePair
  return $ (i, foldl adjust defaultMfcsam [v1, v2, v3])

