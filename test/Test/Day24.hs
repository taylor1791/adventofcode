module Test.Day24 where

import           Day24

import           Test.Tasty
import           Test.Tasty.HUnit

day24 :: TestTree
day24 = testGroup "It Hangs in the Balance" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

sample :: [Int]
sample = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ compartment1 3 sample @?= [9, 11]
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  weights <- map read . lines <$> readFile "input/day24.txt"
  10439961859 @?= product (compartment1 3 weights)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  weights <- map read . lines <$> readFile "input/day24.txt"
  72050269 @?= product (compartment1 4 weights)

