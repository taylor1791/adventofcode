module Test.Day4 where

import Day4 as D4

import Test.Tasty
import Test.Tasty.HUnit

day4 :: TestTree
day4 = testGroup "The Ideal Stocking Stuffer" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ zeroHashed "abcdef" 5 "609043" @?= True
  , testCase "Example 2" $ zeroHashed "pqrstuv" 5 "1048970" @?= True
  ]

p1Puzzle :: TestTree
p1Puzzle = testGroup "Puzzle"
  [
    testCase "Puzzle" $ zeroHashed "ckczppom" 5 "117946" @?= True
  , testCase "Puzzle" $ answer "ckczppom" 5 @?= "117946"
  ]

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testGroup "Puzzle"
  [
    testCase "Puzzle" $ zeroHashed "ckczppom" 6 "3938038" @?= True
  ]
