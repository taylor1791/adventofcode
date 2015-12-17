module Test.Day7 where

import Day7 as D7

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as M
import qualified Data.Word as W

day7 :: TestTree
day7 = testGroup "Some Assembly Required" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

testInput :: [String]
testInput =
  [
    "123 -> x"
  , "456 -> y"
  , "x AND y -> d"
  , "x OR y -> e"
  , "x LSHIFT 2 -> f"
  , "y RSHIFT 2 -> g"
  , "NOT x -> h"
  , "NOT y -> i"
  ]

si :: String -> Source
si = Source . Identifier

parsedInput :: [CML]
parsedInput =
  [
    CML (Id $ Signal 123) (si "x")
  , CML (Id $ Signal 456) (si "y")
  , CML (And (si "x") (si "y")) (si "d")
  , CML (Or (si "x") (si "y")) (si "e")
  , CML (LShift (si "x") (Signal 2)) (si "f")
  , CML (RShift (si "y") (Signal 2)) (si "g")
  , CML (Not (si "x")) (si "h")
  , CML (Not (si "y")) (si "i")
  ]

gmap :: M.Map Identifier W.Word16
gmap = createSource parsedInput

eval :: String -> W.Word16
eval x = evalIdentifier (Identifier x) gmap

answers :: [(String, W.Word16)]
answers =
  [
    ("d", 72)
  , ("e", 507)
  , ("f", 492)
  , ("g", 114)
  , ("h", 65412)
  , ("i", 65079)
  , ("x", 123)
  , ("y", 456)
  ]

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" $
  (testCase "Example 1 - parse" $ map parse testInput @?= parsedInput)
  :
  map (\(l, n) -> testCase ("Example 1 - " ++ l) $ n @?= eval l) answers

p1Puzzle :: TestTree
p1Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  cmls <- fmap (map parse . lines) $ readFile "input/day7.txt"
  46065 @?= evalIdentifier (Identifier "a") (createSource cmls)

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testCaseSteps "Puzzle" $ \_ -> do
  cmls <- fmap (map parse . lines) $ readFile "input/day7.txt"
  let cmls' = CML (Id $ Signal 46065) (si "b") : cmls
  14134 @?= evalIdentifier (Identifier "a") (createSource cmls')

