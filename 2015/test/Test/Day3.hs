module Test.Day3 where

import Day3 as D3

import Test.Tasty
import Test.Tasty.HUnit

day3 :: TestTree
day3 = testGroup "Perfectly Spherical Houses in a Vacuum" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ numHouses ">" @?= 2
  , testCase "Example 2" $ numHouses "^>v<" @?= 4
  , testCase "Example 3" $ numHouses "^v^v^v^v^v" @?= 2
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  n <- fmap numHouses $ readFile "input/day3.txt"
  2572 @?= n

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ numHousesN 2 ">v" @?= 3
  , testCase "Example 2" $ numHousesN 2 "^>v<" @?= 3
  , testCase "Example 3" $ numHousesN 2 "^v^v^v^v^v" @?= 11
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  n <- fmap (numHousesN 2) $ readFile "input/day3.txt"
  2631 @?= n
