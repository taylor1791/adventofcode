module Test.Day9 where

import Day9 as D9

import Test.Tasty
import Test.Tasty.HUnit

day9 :: TestTree
day9 = testGroup "All in a Single Night" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

sample :: [String]
sample = [
    "London to Dublin = 464"
  , "London to Belfast = 518"
  , "Dublin to Belfast = 141"
  ]

parsed :: [((String, String), Int)]
parsed = [
    (("London", "Dublin"), 464)
  , (("London", "Belfast"), 518)
  , (("Dublin", "Belfast"), 141)
  ]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1 - parse" $ parsed @?= map parse sample
  , testCase "Example 1" $ 605 @?= snd (tsp (makeSymmetric parsed))
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  route <- fmap (tsp . makeSymmetric . map parse . lines) $ readFile "input/Day9.txt"
  141 @?= snd route

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  distances <- fmap (makeSymmetric . map parse . lines) $ readFile "input/Day9.txt"
  736 @?= snd (tsp' distances)

