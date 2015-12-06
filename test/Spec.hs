import Test.Tasty

import Test.Day1
import Test.Day2

main :: IO ()
main = defaultMain $ testGroup "Advent of Code" $
  [
    day1
  , day2
  ]

