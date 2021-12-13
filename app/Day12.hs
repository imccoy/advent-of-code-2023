{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

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

class Visitability vis k where
  canVisit :: vis -> k -> Bool
  addVisit :: vis -> k -> vis

newtype CaveOnce = CaveOnce (Set String)

instance Visitability CaveOnce [Char] where
  canVisit (CaveOnce set) cave = not $ Set.member cave set
  addVisit (CaveOnce set) cave = CaveOnce $ Set.insert cave set
emptyCaveOnce = CaveOnce Set.empty

countPaths :: (Visitability vis String) => vis -> String -> Map String [String] -> Int
countPaths _ "end" _ = 1
countPaths vis start graph = let nexts = fromJust . Map.lookup start $ graph
                                 vis' = if isLarge start then vis else addVisit vis start
                                 availableNexts = filter (canVisit vis') nexts
                              in sum . map (\next -> countPaths vis' next graph) $ availableNexts

part1 :: Map String [String] -> IO ()
part1 = putStrLn . show . countPaths emptyCaveOnce "start"

data SuperPower = VisitedSmallCaveTwice

data UsedSuperpower a = Have a | HaveNot a

data OneCaveTwice = OneCaveTwice !(UsedSuperpower SuperPower) !(Set String)

instance Visitability OneCaveTwice [Char]
  where canVisit _                            "start" = False
        canVisit (OneCaveTwice usedPower set) cave = case usedPower of
                                                       Have VisitedSmallCaveTwice -> not $ Set.member cave set
                                                       HaveNot VisitedSmallCaveTwice -> True
        addVisit (OneCaveTwice usedPower set) cave = if Set.member cave set
                                                       then case usedPower of
                                                              Have VisitedSmallCaveTwice -> error "cannot use power twice"
                                                              HaveNot VisitedSmallCaveTwice -> OneCaveTwice (Have VisitedSmallCaveTwice) set
                                                       else OneCaveTwice usedPower (Set.insert cave set)

emptyOneCaveTwice = OneCaveTwice (HaveNot VisitedSmallCaveTwice) Set.empty


part2 :: Map String [String] -> IO ()
part2 = putStrLn . show . countPaths emptyOneCaveTwice "start"

day12 part args = do let filename = case args of
                                      [] -> "inputs/day12"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
