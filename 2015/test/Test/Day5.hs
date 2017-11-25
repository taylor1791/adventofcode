module Test.Day5 where

import Day5 as D5

import Test.Tasty
import Test.Tasty.HUnit

day5 :: TestTree
day5 = testGroup "Doesn't He Have Intern-Elves For This?" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ True @?= D5.nice "ugknbfddgicrmopn"
  , testCase "Example 2" $ True @?= D5.nice "aaa"
  , testCase "Example 3" $ False @?= D5.nice "jchzalrnumimnmhp"
  , testCase "Example 4" $ False @?= D5.nice "haegwjzuvuyypxyu"
  , testCase "Example 5" $ False @?= D5.nice "dvszwmarrgswjxmb"
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  strings <- fmap lines $ readFile "input/day5.txt"
  255 @?= length (filter nice strings)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ True @?= D5.nice2 "qjhvhtzxzqqjkmpb"
  , testCase "Example 2" $ True @?= D5.nice2 "xxyxx"
  , testCase "Example 3" $ False @?= D5.nice2 "aaa"
  , testCase "Example 4" $ False @?= D5.nice2 "uurcxstgmygtbstg"
  , testCase "Example 5" $ False @?= D5.nice2 "ieodomkazucvgmuy"
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  strings <- fmap lines $ readFile "input/day5.txt"
  55 @?= length (filter nice2 strings)

