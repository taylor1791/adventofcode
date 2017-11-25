module Test.Day15 where

import Day15

import Test.Tasty
import Test.Tasty.HUnit

day15 :: TestTree
day15 = testGroup "Science for Hungry People" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

ingrs :: [Ingredient Integer]
ingrs = [
    Ingredient "Butterscotch" (-1) (-2) 6 3 8
  , Ingredient "Cinnamon" 2 3 (-2) (-1) 3
  ]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $ 
  [
    testCase "Example 1" $ 62842880 @?= eval (zip ingrs [44,56])
  , testCase "Example 2" $ 62842880 @?= snd (best 100 ingrs)
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_-> do
  is <- fmap (sequence . map parse . lines) $ readFile "input/day15.txt"
  Right 18965440 @?= fmap (snd . best 100) is

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 57600000 @?= snd (bestCalories 500 100 ingrs)
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_-> do
  is <- fmap (sequence . map parse . lines) $ readFile "input/day15.txt"
  Right 15862900 @?= fmap (snd . bestCalories 500 100) is
