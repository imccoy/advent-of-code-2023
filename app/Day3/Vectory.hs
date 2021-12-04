{-# LANGUAGE BangPatterns #-}
module Day3.Vectory where

import Day3.Common

import Data.List (foldl')
import qualified Data.Vector as Vec

import Part (Part (Part1, Part2))

input :: [String] -> IO [Vec.Vector Bit]
input args = map (Vec.fromList . map charToBit) . lines <$> readFile filename
  where filename = case args of
                     [] -> "inputs/day3"
                     [f] -> f
                     _ -> error "too many arguments"


part1 = do counts <- foldr1 (Vec.zipWith (<>)) . map (fmap bitToCountPair) <$> input []
           let gamma = mostCommonBit <$> counts
           let epsilon = invertBit <$> gamma
           let g = toInt gamma
           let e = toInt epsilon
           putStrLn $ show (g, e, g * e)

searchFor criteria nums0 = go 0 nums0
  where
    go _ [] = error "we have gone too far"
    go _ [bits] = toInt bits
    go !n nums = let currentLeadingDigits = map (Vec.! n) nums
                     counts = foldl' (<>) mempty . map bitToCountPair $ currentLeadingDigits
                     digitToMatch = criteria counts
                     matchingRows = filter ((== digitToMatch) . (Vec.! n)) nums
                  in go (n + 1) matchingRows

part2 args = do nums <- input args
                let oxygen = searchFor mostCommonBit nums
                let co2 = searchFor leastCommonBit nums
                putStrLn $ show (oxygen, co2, oxygen * co2)

day3 Part1 _ = part1
day3 Part2 args = part2 args
