module Test.Day16 where

import Day16

import Test.Tasty
import Test.Tasty.HUnit

day16 :: TestTree
day16 = testGroup "Aunt Sue" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Puzzle]

p1Goal :: MFCSAM
p1Goal = MFCSAM {
    children = Just 3
  , cats = Just 7
  , samoyeds = Just 2
  , pomeranians = Just 3
  , akitas = Just 0
  , vizslas = Just 0
  , goldfish = Just 5
  , trees = Just 3
  , cars = Just 2
  , perfumes = Just 1
  }

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_-> do
  sues <- fmap (sequence . map parse . lines) $ readFile "input/day16.txt"
  Right 40 @?= fmap (fst . (findSue p1Goal)) sues

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_-> do
  sues <- fmap (sequence . map parse . lines) $ readFile "input/day16.txt"
  Right 241 @?= fmap (fst . (findSue' p1Goal)) sues


