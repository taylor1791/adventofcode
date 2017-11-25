module Test.Day19 where

import           Day19

import           Test.Tasty
import           Test.Tasty.HUnit

day19 :: TestTree
day19 = testGroup "Medicine for Rudolph" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

repls1 :: [(String, String)]
repls1 = [("H", "HO"), ("H", "OH"), ("O", "HH")]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ length (generate "HOH" repls1) @?= 4
  , testCase "Example 2" $ length (generate "HOHOHO" repls1) @?= 7
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  Right (dna, replacements) <- parse <$> readFile "input/day19.txt"
  length (generate dna replacements) @?= 535

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

repls2 :: [(String, String)]
repls2 = [("e", "H"), ("e", "O"), ("H", "HO"), ("H", "OH"), ("O", "HH")]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" [
    testCase "Example 1" $ pred (length (reverseFabricate "HOH" repls2)) @?= 3
  , testCase "Example 2" $ pred (length (reverseFabricate "HOHOHO" repls2)) @?= 6
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  Right (dna, replacements) <- parse <$> readFile "input/day19.txt"
  pred (length (reverseFabricate dna replacements)) @?= 212

