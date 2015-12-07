module Day5 where

import Data.Maybe (isJust)
import Text.RegexPR

(=~) :: String -> String -> Bool
x =~ r = isJust $ matchRegexPR r x

nice :: String -> Bool
nice x = vowels x > 2 && isJust (doubleConsecutive x) && sane x

vowels :: String -> Int
vowels = length . filter (`elem` "aeiou")

doubleConsecutive :: String -> Maybe String
doubleConsecutive (x:y:zs)
  | x == y = Just [x,y]
  | otherwise = doubleConsecutive $ y:zs
doubleConsecutive _ = Nothing

sane :: String -> Bool
sane = not . (=~ "(ab|cd|pq|xy)")

nice2 :: String -> Bool
nice2 x = twoParis x && interjected x

twoParis :: String -> Bool
twoParis = (=~ "(..).*\\1")

interjected :: String -> Bool
interjected = (=~ "(.).\\1")

