module Test.Day14 where

import Day14

import Test.Tasty
import Test.Tasty.HUnit

day14 :: TestTree
day14 = testGroup "Reindeer Olympics" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $ 
  [
    testCase "Example 1" $ distance 1000 [(14, 10), (0, 127)] @?= 1120
  , testCase "Example 2" $ distance 1000 [(16, 11), (0, 162)] @?= 1056
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_-> do
  deere <- fmap (sequence . map parse . lines) $ readFile "input/day14.txt"
  Right ("Donner", 2655) @?= fmap ((leadingDeer 2503)) deere

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

rates :: [(String, [(Int, Int)])]
rates = [
    ("Comet", [(14, 10), (0, 127)])
  , ("Dancer", [(16, 11), (0, 162)])
  ]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ bestDeer 1 rates @?= [("Dancer", 1)]
  , testCase "Example 2" $ bestDeer 10 rates @?= [("Dancer", 10)]
  , testCase "Example 3" $ bestDeer 11 rates @?= [("Dancer", 11)]
  , testCase "Example 4" $ bestDeer 1000 rates @?= [("Dancer", 689)]
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_-> do
  deere <- fmap (sequence . map parse . lines) $ readFile "input/day14.txt"
  Right [("Vixen", 1059)] @?= fmap ((bestDeer 2503)) deere

