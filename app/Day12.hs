module Day12 (day12) where

import Data.Char (isAsciiUpper)
import Data.List (partition)
import Data.List.Split (wordsBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

isLarge = all isAsciiUpper

isSemiSmall s = not (isLarge s) && s /= "start" && s /= "end"

parseInput :: String -> Map String [String]
parseInput = Map.unionsWith (<>) . fmap edgesFromLine . lines
  where edgesFromLine line = let [a,b] = wordsBy (== '-') line
                              in Map.fromList [(a,[b]), (b,[a])]

countPaths :: Set String -> String -> Map String [String] -> Int
countPaths _ "end" _ = 1
countPaths avoiding start graph = let nexts = fromJust . Map.lookup start $ graph
                                      availableNexts = filter (not . flip Set.member avoiding) nexts
                                      avoiding' = if isLarge start then avoiding else Set.insert start avoiding
                                   in sum . map (\next -> countPaths avoiding' next graph) $ availableNexts

part1 :: Map String [String] -> IO ()
part1 = putStrLn . show . countPaths Set.empty "start"

countPathsB :: (Bool, Set String) -> String -> Map String [String] -> Int
countPathsB _ "end" _ = 1
countPathsB (smallVisitedTwice, avoiding) start graph = 
  let nexts = fromJust . Map.lookup start $ graph
      (nextsToAvoid, nextsOk) = partition (flip Set.member avoiding) nexts
      avoiding' = if isLarge start then avoiding else Set.insert start avoiding
   in sum $ map (\next -> countPathsB (smallVisitedTwice, avoiding') next graph) nextsOk ++
            if smallVisitedTwice
              then []
              else map (\next -> countPathsB (True, avoiding') next graph) (filter isSemiSmall nextsToAvoid)


part2 :: Map String [String] -> IO ()
part2 = putStrLn . show . countPathsB (False, Set.empty) "start"

day12 part args = do let filename = case args of
                                      [] -> "inputs/day12"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
