{-# LANGUAGE TupleSections, FlexibleContexts, Rank2Types #-}
module Day15 where

import Data.List (maximumBy)

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

bestCalories :: Integer -> Integer -> [Ingredient Integer] -> ([(Ingredient Integer, Integer)], Integer)
bestCalories cal tsp is = maximumBy (\x y -> compare (snd x) (snd y)) $ map (\x -> (x, eval x)) xs
  where
    xs = filter ((cal ==) . totalCalories) $ choices tsp is

best :: Integer -> [Ingredient Integer] -> ([(Ingredient Integer, Integer)], Integer)
best tsp is = maximumBy (\x y -> compare (snd x) (snd y)) $ map (\x -> (x, eval x)) xs
  where
    xs = choices tsp is

choices :: Integer -> [Ingredient Integer] -> [[(Ingredient Integer, Integer)]]
choices n (x:[]) = [[(x, n)]]
choices 0 xs = [map (, 0) xs]
choices n (x:xs) = [(x, i) | i <- [0..n]] >>= (\y -> map (y:) $ choices (n - snd y) xs)

totalCalories :: [(Ingredient Integer, Integer)] -> Integer
totalCalories = foldl (\acc x -> acc + calories x) 0 . map (\(x, n) -> fmap (*n) x)

eval :: [(Ingredient Integer, Integer)] -> Integer
eval = prod . foldl add zero . map (\(x, n) -> fmap (*n) x)
  where
    zero = Ingredient "" 0 0 0 0 0 :: Ingredient Integer

data Ingredient a =
  Ingredient {
    name :: String
  , capacity :: a
  , durability :: a
  , flavor :: a
  , texture :: a
  , calories :: a
  } deriving (
    Show
  )

instance Functor Ingredient where
  fmap f x = Ingredient {
    name = name x
  , capacity = f (capacity x)
  , durability = f (durability x)
  , flavor = f (flavor x)
  , texture = f (texture x)
  , calories = f (calories x)
  }

add :: Num a => Ingredient a -> Ingredient a -> Ingredient a
add x y = Ingredient {
    name = name x ++ " " ++ name y
  , capacity = capacity x + capacity y
  , durability = durability x + durability y
  , flavor = flavor x + flavor y
  , texture = texture x + texture y
  , calories = calories x + calories y
}

prod :: (Ord a, Num a) => Ingredient a -> a
prod x = max' capacity x * max' durability x * max' flavor x * max' texture x
  where
    max' f y = max 0 (f y)

--------------------------------------------------------------------------------

parse :: String -> Either P.ParseError (Ingredient Integer)
parse = P.parse parseIngredient "(string)"

parseIngredient :: Parsec s u m (Ingredient Integer)
parseIngredient = do
  n <- parseIdentifier
  PC.string ": capacity "
  c <- parseInteger
  PC.string ", durability "
  d <- parseInteger
  PC.string ", flavor "
  f <- parseInteger
  PC.string ", texture "
  t <- parseInteger
  PC.string ", calories "
  a <- parseInteger
  return $ Ingredient n c d f t a

parseInteger :: Parsec s u m Integer
parseInteger = P.try parsePosInteger P.<|> P.try parseNegInteger

parsePosInteger :: Parsec s u m Integer
parsePosInteger = read <$> P.many1 P.digit

parseNegInteger :: Parsec s u m Integer
parseNegInteger = do
  P.char '-'
  x <- parsePosInteger
  return $ (-1) * x

parseIdentifier :: Parsec s u m String
parseIdentifier = P.many PC.letter

