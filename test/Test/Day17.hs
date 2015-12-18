module Test.Day17 where

import Day17

import Test.Tasty
import Test.Tasty.HUnit

day17 :: TestTree
day17 = testGroup "No Such Thing as Too Much" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

containers :: [Int]
containers = [20, 15, 10, 5, 5]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ length (possibilities 25 containers) @?= 4
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  sizes <- fmap (sequence . map parse .lines) $ readFile "input/day17.txt"
  Right 654 @?= fmap (length . possibilities 150) sizes

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" [
    testCase "Example 2" $ length (possibilitiesMin 25 containers) @?= 3
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  sizes <- fmap (sequence . map parse .lines) $ readFile "input/day17.txt"
  Right 57 @?= fmap (length . possibilitiesMin 150) sizes

