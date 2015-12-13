module Day11 where

import Data.List (nub, group)
import Data.Maybe (isJust)

nextPass :: String -> String
nextPass = head . dropWhile (not . isValid) . iterate strSucc

isValid :: String -> Bool
isValid x = isJust (increasingStraight x) && not (isConfusing x) && nonOverlappingPairs x

succ2 :: Enum a => a -> a
succ2 = succ . succ

increasingStraight :: String -> Maybe String
increasingStraight (a:b:c:ds)
  | succ2 a == succ b && succ b == c = Just (a:b:c:[])
  | otherwise                        = increasingStraight $ b:c:ds
increasingStraight _ = Nothing

isConfusing :: String -> Bool
isConfusing xs = any id $ "iol" >>= return . (`elem` xs)

nonOverlappingPairs :: String -> Bool
nonOverlappingPairs xs = length (nub groups) > 1
  where
    groupsLong = filter (\x -> length x > 1) $ group xs
    groups = map (take 2) groupsLong

strSucc :: String -> String
strSucc = reverse . strSucc' . reverse
  where
    strSucc' [] = "a"
    strSucc' (x:xs)
      | wrap      = 'a' : strSucc' xs
      | otherwise = succ x : xs
      where
        wrap = succ x == succ 'z'

