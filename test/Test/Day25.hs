module Test.Day25 where

import           Day25

import           Test.Tasty
import           Test.Tasty.HUnit

day25 :: TestTree
day25 = testGroup "Let It Snow" [part1]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ snowMachine (1, 1) @?= 20151125
  , testCase "Example 2" $ snowMachine (2, 1) @?= 31916031
  , testCase "Example 3" $ snowMachine (6, 1) @?= 33071741
  , testCase "Example 3" $ snowMachine (1, 6) @?= 33511524
  , testCase "Example 3" $ snowMachine (6, 6) @?= 27995004
  ]

p1Puzzle :: TestTree
p1Puzzle = testGroup "Test Cases" [
    testCase "Example 1" $ snowMachine (3010, 3019) @?= 8997277
  ]

