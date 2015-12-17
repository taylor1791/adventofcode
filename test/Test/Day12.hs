{-# LANGUAGE OverloadedStrings #-}
module Test.Day12 where

import Day12

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Scientific as S

day12 :: TestTree
day12 = testGroup "JSAbacusFramework.io" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  [
    testCase "Example 1" $ fmap sumNumbers (decode "[1, 2, 3]") @?= Just 6
  , testCase "Example 2" $ fmap sumNumbers (decode "{\"a\":2,\"b\":4}") @?= Just 6
  , testCase "Example 3" $ fmap sumNumbers (decode "[[[3]]]") @?= Just 3
  , testCase "Example 4" $ fmap sumNumbers (decode "{\"a\":{\"b\":4},\"c\":-1}") @?= Just 3
  , testCase "Example 5" $ fmap sumNumbers (decode "{\"a\":[-1,1]}") @?= Just 0
  , testCase "Example 6" $ fmap sumNumbers (decode "[-1,{\"a\":1}]") @?= Just 0
  , testCase "Example 7" $ fmap sumNumbers (decode "[]") @?= Just 0
  , testCase "Example 8" $ fmap sumNumbers (decode "{}") @?= Just 0
  ]

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  puzzle <- fmap decode $ B.readFile "input/day12.txt"
  (Just 111754 :: Maybe S.Scientific) @?= fmap sumNumbers puzzle

part2 :: TestTree
part2 = testGroup "Part 2" [p2Tests, p2Puzzle]

p2Tests :: TestTree
p2Tests = testGroup "Test Cases"
  [
    testCase "Example 1" $ fmap sumNonRedNumbers (decode "[1,2,3]") @?= Just 6
  , testCase "Example 2" $ fmap sumNonRedNumbers (decode "[1,{\"c\":\"red\",\"b\":2},3]") @?= Just 4
  , testCase "Example 3" $ fmap sumNonRedNumbers (decode "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") @?= Just 0
  , testCase "Example 4" $ fmap sumNonRedNumbers (decode "[1, \"red\", 5]") @?= Just 6
  ]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  puzzle <- fmap decode $ B.readFile "input/day12.txt"
  (Just 65402 :: Maybe S.Scientific) @?= fmap sumNonRedNumbers puzzle

