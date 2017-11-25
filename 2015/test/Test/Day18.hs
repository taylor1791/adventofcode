{-# LANGUAGE FlexibleContexts #-}
module Test.Day18 where

import           Day18

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Array.Unboxed

day18 :: TestTree
day18 = testGroup "Like a GIF For Your Yard" [part1, part2]

example :: Array Pos Value
example = parse . lines $ ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."

steps :: [Array Pos Value]
steps = iterate tick example

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ count (steps !! 1) @?= 11
  , testCase "Example 2" $ count (steps !! 2) @?= 8
  , testCase "Example 3" $ count (steps !! 3) @?= 4
  , testCase "Example 4" $ count (steps !! 4) @?= 4
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  board <- fmap (parse . lines) $ readFile "input/day18.txt"
  count (iterate tick board !! 100) @?= 821

steps' :: [Array Pos Value]
steps' = iterate tick' (lightCorners example)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases" [
    testCase "Example 1" $ count (steps' !! 1) @?= 18
  , testCase "Example 2" $ count (steps'!! 2) @?= 18
  , testCase "Example 3" $ count (steps'!! 3) @?= 18
  , testCase "Example 4" $ count (steps'!! 4) @?= 14
  , testCase "Example 5" $ count (steps'!! 5) @?= 17
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  board <- fmap (parse . lines) $ readFile "input/day18.txt"
  count (iterate tick' (lightCorners board) !! 100) @?= 886

