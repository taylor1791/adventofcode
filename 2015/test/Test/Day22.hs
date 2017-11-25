module Test.Day22 where

import           Day22

import           Data.Function    (on)
import           Data.List        (minimumBy)

import           Test.Tasty
import           Test.Tasty.HUnit

day22 :: TestTree
day22 = testGroup "Wizard Simulator 20XX" [part1, part2]

part1 :: TestTree
part1 = testGroup "Part 1" [p1Tests, p1Puzzle]

spells1 :: [Spell]
spells1 = [Poison, Missile]

p11Start :: (Player, Boss)
p11Start = (Player 10 250 [], Boss 13 8 [])

p11End :: (Player, Boss)
p11End = (Player 2 24 [], Boss 0 8 [EPoison 3])

spells2 :: [Spell]
spells2 = [Recharge, Shield, Drain, Poison, Missile]

p12Start :: (Player, Boss)
p12Start = (Player 10 250 [], Boss 14 8 [])

p12End :: (Player, Boss)
p12End = (Player 1 114 [], Boss 0 8 [EPoison 3])

p1Tests :: TestTree
p1Tests = testGroup "Test Cases" [
    testCase "Example 1" $ play p11Start spells1 @?= p11End
  , testCase "Example 2" $ play p12Start spells2 @?= p12End
  ]

start :: (Player, Boss)
start = (Player 50 500 [], Boss 71 10 [])

isCandidate :: [Spell] -> Bool
isCandidate x = isGameOver x' && playerWins x'
  where
    x' = play start x

candidates :: [[Spell]]
candidates = filter isCandidate $ concatMap spells [10..12]

p1Puzzle :: TestTree
p1Puzzle = testGroup "Puzzle" [
    testCase "Puzzle" $ 1824 @?= cost (minimumBy (compare `on` cost) candidates)
  ]

candidates' :: [[Spell]]
candidates' = filter isCandidate' $ concatMap spells [13]

isCandidate' :: [Spell] -> Bool
isCandidate' x = isGameOver x' && playerWins x'
  where
    x' = play' start x

part2 :: TestTree
part2 = testGroup "Part 2" [p2Puzzle]

p2Puzzle :: TestTree
p2Puzzle = testGroup "Puzzle" [
    testCase "Puzzle" $ 1937 @?= cost (minimumBy (compare `on` cost) candidates')
  ]
