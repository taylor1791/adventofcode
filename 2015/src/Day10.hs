module Day10 where

import Data.List (group, foldl1')

lookSayN :: Int -> String -> String
lookSayN n = foldl1' (.) $ take n $ repeat (lookSay . group)

lookSay :: [String] -> String
lookSay [] = []
lookSay (x:xs) = c1 : c2 : lookSay xs
  where
    (c1:[]) = show $ length x
    c2 = head x

