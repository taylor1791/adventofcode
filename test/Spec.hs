import Test.Tasty

import Test.Day1

main :: IO ()
main = defaultMain $ testGroup "Advent of Code" $
  [
    day1
  ]

