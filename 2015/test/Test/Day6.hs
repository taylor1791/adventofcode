module Test.Day6 where

import Data.Array.Unboxed

import Day6 as D6

import Test.Tasty
import Test.Tasty.HUnit

day6 :: TestTree
day6 = testGroup "Probably a Fire Hazard" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

lights' :: Array (Int, Int) Bool -> Int
lights' = lights

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 1000000 @?= lights' (lightLights [On (0, 0) (999, 999)])
  , testCase "Example 1 - parse" $ On (0, 0) (999, 999) @?= parse "turn on 0,0 through 999,999"
  , testCase "Example 2" $  999000 @?= lights' (lightLights [On (0, 0) (999, 999), Toggle (0, 0) (999, 0)])
  , testCase "Example 2 - parse" $ Toggle (0, 0) (999, 999) @?= parse "toggle 0,0 through 999,999"
  , testCase "Example 3" $  999996 @?= lights' (lightLights [On (0, 0) (999, 999), Off (499, 499) (500, 500)])
  , testCase "Example 3 - parse" $ Off (0, 0) (999, 999) @?= parse "turn off 0,0 through 999,999"
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  instructions <- fmap (map parse . lines) $ readFile "input/day6.txt"
  569999 @?= lights' (lightLights instructions)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

lights'' :: Array (Int, Int) Int -> Int
lights'' = lights

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ 1 @?= lights'' (lightLights [On (0, 0) (0, 0)])
  , testCase "Example 2" $ 2000000 @?= lights'' (lightLights [Toggle (0, 0) (999, 999)])
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  instructions <- fmap (map parse . lines) $ readFile "input/day6.txt"
  17836115 @?= lights'' (lightLights instructions)

