module Test.Day21 where

import           Day21

import           Test.Tasty
import           Test.Tasty.HUnit

day21 :: TestTree
day21 = testGroup "RPG Simulator 20XX" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ playerWins (Player 8 (Equipment "" 0 5 0) (Just $ Equipment "" 0 0 5) []) (Boss 12 7 2) @?= True
  ]

boss :: Boss
boss = Boss 100 8 2

p1Puzzle :: TestTree
p1Puzzle = testGroup "Puzzle" [
    testCase "Puzzle" $ 91 @?= minimum (map playerCost (winningEquipmentSets player boss))
  ]

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testGroup "Puzzle" [
    testCase "Puzzle" $ 158 @?= maximum (map playerCost (losingEquipmentSets player boss))
  ]
