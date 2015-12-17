module Test.Day13 where

import Day13

import Test.Tasty
import Test.Tasty.HUnit

day13 :: TestTree
day13 = testGroup "Knights of the Dinner Table" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

sampleInput :: [String]
sampleInput = [
    "Alice would gain 54 happiness units by sitting next to Bob."
  , "Alice would lose 79 happiness units by sitting next to Carol."
  , "Alice would lose 2 happiness units by sitting next to David."
  , "Bob would gain 83 happiness units by sitting next to Alice."
  , "Bob would lose 7 happiness units by sitting next to Carol."
  , "Bob would lose 63 happiness units by sitting next to David."
  , "Carol would lose 62 happiness units by sitting next to Alice."
  , "Carol would gain 60 happiness units by sitting next to Bob."
  , "Carol would gain 55 happiness units by sitting next to David."
  , "David would gain 46 happiness units by sitting next to Alice."
  , "David would lose 7 happiness units by sitting next to Bob."
  , "David would gain 41 happiness units by sitting next to Carol. "
  ]

sample :: [((String, String), Int)]
sample = [
   (("Alice", "Bob"  ),  54)
  ,(("Alice", "Carol"), -79)
  ,(("Alice", "David"),  -2)
  ,(("Bob"  , "Alice"),  83)
  ,(("Bob"  , "Carol"),  -7)
  ,(("Bob"  , "David"), -63)
  ,(("Carol", "Alice"), -62)
  ,(("Carol", "Bob"  ),  60)
  ,(("Carol", "David"),  55)
  ,(("David", "Alice"),  46)
  ,(("David", "Bob"  ),  -7)
  ,(("David", "Carol"),  41)
  ]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1 - Parse" $ Right sample @?= (sequence . map parse) sampleInput
  , testCase "Example 1" $ 330 @?= (snd . optimize) sample
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_-> do
  happiness <- fmap (sequence . map parse . lines) $ readFile "input/day13.txt"
  Right 709 @?= fmap (snd . optimize) happiness

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_-> do
  happiness <- fmap (fmap (addApetite "Hillary") . sequence . map parse . lines) $ readFile "input/day13.txt"
  Right 668 @?= fmap (snd . optimize) happiness

