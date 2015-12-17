module Test.Day8 where

import Day8 as D8

import Test.Tasty
import Test.Tasty.HUnit

day8 :: TestTree
day8 = testGroup "Matchsticks" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 2 @?= charDiff (show "")
  , testCase "Example 2" $ 2 @?= charDiff (show "abc")
  , testCase "Example 3" $ 3 @?= charDiff (show "aaa\"aaa")
  , testCase "Example 4" $ 2 @?= charDiff "\"\\27\""
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  puzzle <- fmap lines $ readFile "input/Day8.txt"
  1333 @?= sum (map charDiff puzzle)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 4 @?= charIncr 0 (show "")
  , testCase "Example 2" $ 4 @?= charIncr 0 (show "abc")
  , testCase "Example 3" $ 6 @?= charIncr 0 (show "aaa\"aaa")
  , testCase "Example 4" $ 5 @?= charIncr 0 "\"\\27\""
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  puzzle <- fmap lines $ readFile "input/Day8.txt"
  2046 @?= sum (map (charIncr 0) puzzle)

