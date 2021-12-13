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

data CaveAlways = CaveAlways

instance Visitability CaveAlways [Char] where
  canVisit _ _ = True
  addVisit _ _ = CaveAlways

newtype CaveOnce = CaveOnce (Set String)

instance Visitability CaveOnce [Char] where
  canVisit (CaveOnce set) cave = not $ Set.member cave set
  addVisit (CaveOnce set) cave = CaveOnce $ Set.insert cave set

data CaveLittleBig l b = CaveLittleBig !l !b

instance (Visitability l String, Visitability b String) => Visitability (CaveLittleBig l b) String where
  canVisit (CaveLittleBig l b) cave | isLarge cave = canVisit b cave
                                    | otherwise    = canVisit l cave
  addVisit (CaveLittleBig l b) cave | isLarge cave = CaveLittleBig l (addVisit b cave)
                                    | otherwise    = CaveLittleBig (addVisit l cave) b

emptyCaveOnce = CaveOnce Set.empty

countPaths :: (Visitability vis String) => vis -> String -> Map String [String] -> Int
countPaths vis start graph = search (addVisit vis start) start
  where
    search vis "end" = 1
    search vis start = sum .
                          map (\next -> search (addVisit vis next) next) .
                          filter (canVisit vis) .
                          fromJust $
                          Map.lookup start graph

part1 :: Map String [String] -> IO ()
part1 = putStrLn . show . countPaths (CaveLittleBig emptyCaveOnce CaveAlways) "start"

data SuperPower = VisitedSmallCaveTwice

data UsedSuperpower a = Have !a | HaveNot !a

data OneCaveTwice v = OneCaveTwice !(UsedSuperpower SuperPower) !v

instance (Visitability v String) => Visitability (OneCaveTwice v) String
  where canVisit _                          "start" = False
        canVisit (OneCaveTwice usedPower v) cave = case usedPower of
                                                     Have VisitedSmallCaveTwice -> canVisit v cave
                                                     HaveNot VisitedSmallCaveTwice -> True
        addVisit (OneCaveTwice usedPower v) cave = if not $ canVisit v cave
                                                     then case usedPower of
                                                            Have VisitedSmallCaveTwice -> error "cannot use power twice"
                                                            HaveNot VisitedSmallCaveTwice -> OneCaveTwice (Have VisitedSmallCaveTwice) v
                                                     else OneCaveTwice usedPower (addVisit v cave)

part2 :: Map String [String] -> IO ()
part2 = putStrLn . show . countPaths (CaveLittleBig (OneCaveTwice (HaveNot VisitedSmallCaveTwice) emptyCaveOnce) CaveAlways) "start"

day12 part args = do let filename = case args of
                                      [] -> "inputs/day12"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
