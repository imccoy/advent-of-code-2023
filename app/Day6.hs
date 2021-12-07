module Day6 (day6) where
import Part (Part (Part1, Part2))

import Data.List (foldl')
import Data.List.Split (splitWhen)

input :: IO [Int]
input =  map read .splitWhen (== ',') <$> readFile "inputs/day6"

countAges :: [Int] -> [Integer]
countAges ages = [fromIntegral . length . filter (== n) $ ages | n <- [0..maxAge]]
  where maxAge :: Int
        maxAge = foldl' max 0 ages

iterateSpawnCounts initial = go (sum initial) paddedInitial
  where go total [n0,n1,n2,n3,n4,n5,n6,n7,n8] = (total + n0):(go (total + n0) [n1,n2,n3,n4,n5,n6,n7+n0,n8,n0])
        paddedInitial = initial ++ replicate (9 - length initial) 0

part1 ages = do
  putStrLn . show . countAges $ ages
  putStrLn . show . take 40 . iterateSpawnCounts . countAges $ ages
  putStrLn . show . head . drop 17 . iterateSpawnCounts . countAges $ ages
  putStrLn . show . head . drop 79 . iterateSpawnCounts . countAges $ ages

part2 = do
  ages <- input
  putStrLn . show . head . drop 255 . iterateSpawnCounts . countAges $ ages

day6 Part1 _ = do
  putStrLn "0 1 1 2 1 0 0 0 1 1 3 2 2 1 0 1 1 4 3"
  part1 [3,4,3,1,2]
  part1 =<< input
day6 Part2 _ = part2
