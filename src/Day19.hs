{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day19 where

import           Data.List    (nub, isPrefixOf)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

-- See https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
-- for a good solution.

-- This is a backwards BFS, it is much faster than a forwards one
reverseFabricate :: String -> [(String, String)] -> [String]
reverseFabricate s replacements = fromMaybe (error "No Solution") $ go [] s
  where
    replacements' = map (\(x, y) -> (y, x)) replacements
    go a "e" = Just $ "e":a
    go a x
      | null children = Nothing
      | otherwise     = listToMaybe . mapMaybe (go (x:a)) $ children
      where
        children = generate x replacements'
      
-- This uses a really bad version of bfs, turns out not to be good enough
-- fabricate :: String -> [(String, String)] -> [String]
-- fabricate goal replacements = go [["e"]]
--   where
--     go genes = case find ((== goal) . head) genes of
--       Just x -> x
--       _      -> go $ genes >>= (\x -> map (:x) $ generate (head x) replacements)

generate :: String -> [(String, String)] -> [String]
generate s = nub . concatMap (replacement s)

replacement :: String -> (String, String) -> [String]
replacement s (regex, splice) = map (replace splice) $ multiMatch regex s

replace :: String -> (String, (String, String)) -> String
replace y (_, (x, z)) = x ++ y ++ z

multiMatch :: String -> String -> [(String, (String, String))]
multiMatch needle = go []
  where
    l = length needle
    go _ [] = []
    go prefix haystack@(h:s)
      | needle `isPrefixOf` haystack = (needle, (reverse prefix, drop l haystack)) : go (h:prefix) s
      | otherwise                    = go (h:prefix) s

parse :: String -> Either P.ParseError (String, [(String, String)])
parse = P.parse parseReplacement "(string)"

parseIdentifier :: Parsec s u m String
parseIdentifier = P.many PC.letter

parseReplacement :: Parsec s u m (String, [(String, String)])
parseReplacement = do
  replacements <- P.many parseLine
  PC.newline
  dna <- parseIdentifier
  return (dna, replacements)

parseLine :: Parsec s u m (String, String)
parseLine = do
  x <- parseIdentifier
  P.string " => "
  y <- parseIdentifier
  PC.newline
  return (x, y)
