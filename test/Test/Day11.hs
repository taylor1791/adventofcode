module Test.Day11 where

import Day11

import Test.Tasty
import Test.Tasty.HUnit

day11 = testGroup "Corporate Policy" [part1]

part1 = testGroup "Part 1" [p1Tests, p1Puzzle]
p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ increasingStraight "hijklmmn" @?= Just "hij"
  , testCase "Example 2" $ isConfusing "hijklmmn" @?= True
  , testCase "Example 3" $ isValid "hijklmmn" @?= False
  , testCase "Example 4" $ increasingStraight "abbceffg" @?= Nothing
  , testCase "Example 5" $ nonOverlappingPairs "abbceffg" @?= True
  , testCase "Example 6" $ isValid "abbceffg" @?= False
  , testCase "Example 7" $ nonOverlappingPairs "abbcegjk" @?= False
  , testCase "Example 8" $ nextPass "abcdefgh" @?= "abcdffaa"
  , testCase "Example 9" $ nextPass "ghijklmn" @?= "ghjaabcc"
  ]

p1Puzzle :: TestTree
p1Puzzle = testGroup "Puzzle" 
  [
    testCase "Puzzle" $ nextPass "hepxcrrq" @?= "hepxxyzz"
  , testCase "Puzzle" $ nextPass (strSucc "hepxxyzz") @?= "heqaabcc"
  ]
