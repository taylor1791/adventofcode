module Test.Day1 where

import Data.List (foldl1')

import Day1 as D1

import Test.Tasty
import Test.Tasty.HUnit

day1Examples =
  [
    (0, ["(())", "()()"])
  , (3, ["(((", "(()(()("])
  , (3, ["))((((("])
  , (-1, ["())", "))("])
  , (-3, [")))", ")())())"])
  ]

reason = ("All values are not equal to " ++)

same xs = null $ dropWhile (== head xs) xs

createTest (n, (ans, strs)) =
  testCase ("Example " ++ show n) $ assertBool (reason $ show ans) $
    same (ans : (map D1.floor strs))

part1 = testGroup "Part 1" [day1p1Tests, day1p1Puzzle]

day1p1Tests :: TestTree
day1p1Tests = testGroup "Test Cases" $
  map createTest $
  zip [1..] day1Examples

day1p1Puzzle :: TestTree
day1p1Puzzle = testCaseSteps "Puzzle" $ \step -> do
  string <- readFile "input/day1.txt"
  138 @?= D1.floor string

part2 = testGroup "Part 2" [day1p2Tests, day1p2Puzzle]

day1p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 1 @?= D1.basement ")"
  , testCase "Example 2" $ 5 @?= D1.basement "()())"
  ]

day1p2Puzzle = testCaseSteps "Puzzle" $ \step -> do
  string <- readFile "input/day1.txt"
  1771 @?= D1.basement string

day1 = testGroup "Not Quite Lisp" [part1, part2]
