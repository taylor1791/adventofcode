module Test.Day2 where

import Data.List (foldl1')

import Day2 as D2

import Test.Tasty
import Test.Tasty.HUnit

day2 = testGroup "I Was Told There Would Be No Math" [part1, part2]

part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1 - parse" $ D2.parse "2x3x4" @?= RRP 2 3 4
  , testCase "Example 1" $ 58 @?= D2.paper (RRP 2 3 4)
  , testCase "Example 2 - parse" $ D2.parse "1x1x10" @?= RRP 1 1 10
  , testCase "Example 2" $ 43 @?= D2.paper (RRP 1 1 10)
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \step -> do
  dimensions <- fmap (map parse . lines) $ readFile "input/day2.txt"
  1598415 @?= sum (map paper dimensions)

part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 34 @?= ribbon (RRP 2 3 4)
  , testCase "Example 2" $ 14 @?= ribbon (RRP 1 1 10)
  ]

p2Puzzle = testCaseSteps "Puzzle" $ \step -> do
  dimensions <- fmap (map parse . lines) $ readFile "input/day2.txt"
  3812909 @?= sum (map ribbon dimensions)

