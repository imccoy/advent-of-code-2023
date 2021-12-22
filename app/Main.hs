module Main where

import Data.Maybe
import System.Environment (getArgs)
import System.Exit (die)

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
import Day20 (day20)
import Day21 (day21)
-- NEXT DAY IMPORT

import Part (getPart, Part (Part1, Part2))

getDay :: String -> Maybe (Part -> [String] -> IO ())
getDay n =
  case n of
    "1" -> Just day1
    "2" -> Just day2
    "3" -> Just day3
    "4" -> Just day4
    "5" -> Just day5
    "6" -> Just day6
    "7" -> Just day7
    "8" -> Just day8
    "9" -> Just day9
    "10" -> Just day10
    "11" -> Just day11
    "12" -> Just day12
    "13" -> Just day13
    "14" -> Just day14
    "15" -> Just day15
    "16" -> Just day16
    "17" -> Just day17
    "18" -> Just day18
    "19" -> Just day19
    "20" -> Just day20
    "21" -> Just day21
    -- NEXT DAY CASE
    _ -> Nothing


main :: IO ()
main = do
  args <- getArgs
  case args of
    (day:part:rest) -> 
      do
        day' <- fromMaybe (die $ "Unknown day " ++ day) (pure <$> getDay day)
        part' <- fromMaybe (die $ "Unknown part " ++ part) (pure <$> getPart part)
        (day' :: Part -> [String] -> IO ()) part' rest
    _ -> putStrLn "usage: <cmd> <day> <part>"
