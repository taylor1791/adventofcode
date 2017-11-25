module Test.Day20 where

import           Day20

import           Test.Tasty
import           Test.Tasty.HUnit

day20 :: TestTree
day20 = testGroup "Infinite Elves and Infinite Houses" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ map presents [1..9] @?= [10, 30, 40, 70, 60, 120, 80, 150, 130]
  ]

p1Puzzle :: TestTree
p1Puzzle = testGroup "Puzzle" [
    testCase "Puzzle" $ head (dropWhile ((< 29000000) . presents) [1..]) @?= 665280
  ]

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testGroup "Puzzle" [
    testCase "Puzzle" $ head (dropWhile ((< 29000000) . presents') [1..]) @?= 705600
  ]

