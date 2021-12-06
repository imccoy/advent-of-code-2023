module Day5 where
import Part (Part (Part1, Part2))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitWhen, wordsBy)

type Line = ((Int,Int),(Int,Int))

input :: IO [((Int,Int),(Int,Int))]
input = fmap (parseLine . words) . lines <$> readFile "inputs/day5"
  where parseLine (a:_:b:[]) = (parsePoint a, parsePoint b)
        parsePoint point = let [a,b] = splitWhen (== ',') point
                            in (read a, read b)

horizontalLine ((x0,y0),(x1,y1)) = y0 == y1
verticalLine ((x0,y0),(x1,y1)) = x0 == x1

linePoints ((x0,y0),(x1,y1)) = [(x0 + d * xd, y0 + d * yd) | d <- [0..len]]
  where xd = if x0 < x1 then 1 else (if x0 == x1 then 0 else -1)
        yd = if y0 < y1 then 1 else (if y0 == y1 then 0 else -1)
        len = abs (if x0 == x1 then y1 - y0 else x1 - x0)
countPoints :: [Line] -> Map (Int,Int) Int
countPoints = foldr (\l map -> addLinePoints l map) Map.empty
  where
    addLinePoints line map = foldr (\p map -> Map.alter increment p map) map (linePoints line)
    increment Nothing = Just 1
    increment (Just x) = let n = x + 1 in n `seq` Just n

part1 = putStrLn =<< show . length . filter (> 1) . Map.elems . countPoints . filter (\l -> horizontalLine l || verticalLine l) <$> input
part2 = putStrLn =<< show . length . filter (> 1) . Map.elems . countPoints <$> input

{-
import Data.Set (Set)
import qualified Data.Set as Set

x0 ((n,_),(_,_)) = n
x1 ((_,_),(n,_)) = n
y0 ((_,n),(_,_)) = n
y1 ((_,_),(_,n)) = n

crossingPoints :: [Line] -> Set (Int,Int)
crossingPoints [] = Set.empty
crossingPoints (l:ls) = foldr (\l' set -> addIntersectingPoints l l' set) (crossingPoints ls) ls
  where
    addIntersectingPoints a b set
      | horizontalLine a && verticalLine b 
          && inRange (x0 b) (x0 a, x1 a)
          && inRange (y0 a) (y0 b, y1 b) = Set.insert (x0 b, y0 a) set
      | horizontalLine b && verticalLine a
          && inRange (x0 a) (x0 b, x1 b)
          && inRange (y0 b) (y0 a, y1 a) = Set.insert (x0 a, y0 b) set
      | horizontalLine a && horizontalLine b
          && y0 a == y0 b                = case overlapping (x0 a, x1 a) (x0 b, x1 b) of
                                             Just (start, end) -> set <> Set.fromList [(x,y0 a) | x <- [start..end]]
                                             Nothing -> set
      | verticalLine a && verticalLine b
          && x0 a == x0 b                = case overlapping (y0 a, y1 a) (y0 b, y1 b) of
                                             Just (start, end) -> set <> Set.fromList [(x0 a,y) | y <- [start..end]]
                                             Nothing -> set
      | otherwise                   = set

inRange n (start, end) | start < end = n >= start && n <= end
                       | otherwise   = n >= end && n <= start

overlapping (a0, a1) (b0, b1)
  | a0 > a1   = overlapping (a1, a0) (b0, b1)
  | b0 > b1   = overlapping (a0, a1) (b1, b0)
  | a0 <= b0 && a1 >= b0  = Just (b0, min a1 b1)
  | a0 >= b0 && a1 <= b1  = Just (a0, min a1 b1)
  | b0 <= a0 && b1 >= a0  = Just (a0, min a1 b1)
  | b0 >= a0 && b1 <= a1  = Just (b0, min a1 b1)
  | otherwise             = Nothing

part1 = putStrLn =<< show . Set.size . crossingPoints <$> input

-- |    /
-- | o /
-- |  /
-- | /  v 
-- |/
-----------------



crossingPointsDiagonal :: [Line] -> Set (Int,Int)
crossingPointsDiagonal = crossingPoints . map rotate
  where
    rotate (a, b) = (rotatePoint a, rotatePoint b)
    rotatePoint (x, y) | x == y = (x, 0)
                       | x < y  = let distanceToNewX = y - x
                                      pointOnNewX = (x + distanceToNewX, y - distanceToNewX)
                                   in ( fst pointOnNewX
                                      , distanceToNewX)
                       | x > y  = let distanceToNewX = x - y
                                      pointOnNewX = (x - distanceToNewX, y + distanceToNewX)
                                   in ( fst pointOnNewX
                                      , -distanceToNewX)

part2 = putStrLn =<< show . Set.size . (\x -> crossingPoints x <> crossingPointsDiagonal x) <$> input

-}

day5 Part1 _ = part1
day5 Part2 _ = part2
