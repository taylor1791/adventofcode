{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Day2 where

import Prelude hiding (length)

import qualified Text.Parsec as P
import Text.Parsec.Char (char)

data RRP = RRP
  {
    length :: Int
  , width :: Int
  , height :: Int
  } deriving (
    Eq
  , Show
  )

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

parse :: String -> RRP
parse x = case P.parse parseRRP "(source)" x of
  Left _ -> error $ "Failed to parse " ++ show x
  Right e -> e

parseRRP :: Parsec s u m RRP
parseRRP = do
  l <- readIntX
  w <- readIntX
  h <- readInt
  return $ RRP l w h

readIntX :: Parsec s u m Int
readIntX = readInt >>= \x -> readX >> return x

readInt :: Parsec s u m Int
readInt = read <$> P.many1 P.digit

readX :: Parsec s u m Char
readX = char 'x'

paper :: RRP -> Int
paper x = 2 * (a1 + a2 + a3) + minimum [a1, a2, a3]
  where
    a1 = length x * width x
    a2 = width x * height x
    a3 = height x * length x

ribbon :: RRP -> Int
ribbon x = bow + 2 * minimum [p1, p2, p3]
  where
    bow = length x * width x * height x
    p1 = length x + width x
    p2 = width x + height x
    p3 = height x + length x
