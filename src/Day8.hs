{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Day8 where

  charDiff :: String -> Int
  charDiff ('"':ys) = charDiff' (1 :: Int) ys
    where
      charDiff' n (['"']) = n + 1
      charDiff' n ('\\':'x':_:_:xs) = charDiff' (n + 3) xs
      charDiff' n ('\\':'\\':xs) = charDiff' (n + 1) xs
      charDiff' n ('\\':'"':xs) = charDiff' (n + 1) xs
      charDiff' n (_:xs) = charDiff' n xs

  charIncr :: Int -> String -> Int
  charIncr n [] = n + 2
  charIncr n ('"':xs) = charIncr (n + 1) xs
  charIncr n ('\\':xs) = charIncr (n + 1) xs
  charIncr n (_:xs) = charIncr n xs

