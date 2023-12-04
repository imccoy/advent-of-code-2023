module Day1 (day1) where

import Part (Part (Part1, Part2))
import Data.Char
import Data.List
import Control.Lens
import Control.Monad

input :: IO String
input = readFile "inputs/day1"

inputLines :: IO [String]
inputLines = lines <$> input

numsFromLine :: String -> [Int]
numsFromLine = map (read . pure) . filter isDigit

firstLastNums line = (head $ numsFromLine line) * 10 + (head . reverse $ numsFromLine line)

part1 = do
  putStrLn =<< show . sum . fmap firstLastNums <$> inputLines

numPrefix :: [(String, Int)] -> String -> [Int]
numPrefix strints = go
  where go [] = []
        go (c:s) = if isDigit c
                     then (read [c]):(go s)
                     else (do (prefix, val) <- strints
                              if prefix `isPrefixOf` (c:s) then pure val else [] 
                          ) ++ go s

numPrefixBack :: [(String, Int)] -> String -> [Int]
numPrefixBack strints s = numPrefix ((_1 %~ reverse) <$> strints) (reverse s)

wordmap = [("one",1),("two",2),("three",3),("four",4),("five",5),("six",6),("seven",7),("eight",8),("nine",9)]

firstLastSnums line = (head $ numPrefix wordmap line) * 10 + (head $ numPrefixBack wordmap line)

part2 = do
  putStrLn =<< show . sum . fmap firstLastSnums <$> inputLines

day1 Part1 _ = part1
day1 Part2 _ = part2
