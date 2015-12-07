import Test.Tasty

import Test.Day1
import Test.Day2
import Test.Day3
import Test.Day4

main :: IO ()
main = defaultMain $ testGroup "Advent of Code" $
  [
    day1
  , day2
  , day3
  , day4
  ]

