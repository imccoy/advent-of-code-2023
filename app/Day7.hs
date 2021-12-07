module Day7 (day7) where

import Part (Part (Part1, Part2))

import Data.Foldable (maximum, minimum)
import Data.List.Split (splitWhen)

readCrabLocations :: IO [Int]
readCrabLocations =  map read .splitWhen (== ',') <$> readFile "inputs/day7"

run cost = do crabLocations <- readCrabLocations
              let costs = map (\location -> sum . map (cost location) $ crabLocations)
                              [0..maximum crabLocations]
              pure . minimum $ costs

day7 Part1 _ = putStrLn . show =<< run (\start end -> abs (start - end))
day7 Part2 _ = putStrLn . show =<< run (\start end -> let n = abs (start - end)
                                                       in n * (n + 1) `div` 2)
