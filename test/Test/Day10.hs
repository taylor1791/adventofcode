module Test.Day10 where

import Day10 as D10

import Test.Tasty
import Test.Tasty.HUnit

day10  :: TestTree
day10 = testGroup "Elves Look, Elves Say" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ lookSayN 1 "1" @?= "11"
  , testCase "Example 2" $ lookSayN 1 "11" @?= "21"
  , testCase "Example 3" $ lookSayN 1 "21" @?= "1211"
  , testCase "Example 4" $ lookSayN 1 "1211" @?= "111221"
  , testCase "Example 5" $ lookSayN 1 "111221" @?= "312211"
  , testCase "Example 6" $ lookSayN 5 "1" @?= "312211"
  ]

p1Puzzle :: TestTree
p1Puzzle = testGroup "Puzzle"
  [
    testCase "Puzzle" $ length (lookSayN 40 "3113322113")
      @?= 329356
  ]

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testGroup "Puzzle"
  [
    testCase "Puzzle" $ length (lookSayN 50 "3113322113")
      @?= 4666278
  ]

