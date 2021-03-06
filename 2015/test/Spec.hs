{-# OPTIONS_GHC -F -pgmF test-pre #-}
import           Test.Tasty

import           Test.Day1
import           Test.Day10
import           Test.Day11
import           Test.Day12
import           Test.Day13
import           Test.Day14
import           Test.Day15
import           Test.Day16
import           Test.Day17
import           Test.Day18
import           Test.Day19
import           Test.Day2
import           Test.Day20
import           Test.Day21
import           Test.Day22
import           Test.Day23
import           Test.Day24
import           Test.Day25
import           Test.Day3
import           Test.Day4
import           Test.Day5
import           Test.Day6
import           Test.Day7
import           Test.Day8
import           Test.Day9

main :: IO ()
main = defaultMain $ testGroup "Advent of Code"
  [
    day1
  -- , day2
  -- , day3
  -- , day4
  -- , day5
  -- , day6
  -- , day7
  -- , day8
  -- , day9
  -- , day10
  -- , day11
  -- , day12
  -- , day13
  -- , day14
  -- , day15
  -- , day16
  -- , day17
  -- , day18
  -- , day19
  -- , day20
  -- , day21
  -- , day22
  -- , day23
  -- , day24
  , day25
  ]

