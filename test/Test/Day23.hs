module Test.Day23 where

import           Day23

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Vector      as V

day23 :: TestTree
day23 = testGroup "Opening the Turing Lock" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

sample :: V.Vector Instr
sample = V.fromList [Inc A, Jio A 2, Tpl A, Inc A]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ a (run sample Day23.init) @?= 2
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  Right program <- parse <$> readFile "input/day23.txt"
  170 @?= b (run program Day23.init)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  Right program <- parse <$> readFile "input/day23.txt"
  247 @?= b (run program (Machine 0 1 0))

