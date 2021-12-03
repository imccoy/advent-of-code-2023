module Day3 where

import Data.Bits
import Data.List (foldl')
import Debug.Trace

import Part (Part (Part1, Part2))

input :: [String] -> IO [[Int]]
input args = map (map charToBit) . lines <$> readFile filename
  where filename = case args of
                     [] -> "inputs/day3"
                     [f] -> f
                     _ -> error "too many arguments"

data CountPair = CountPair { zeros :: !Int, ones :: !Int }

instance (Semigroup CountPair) where
  a <> b = CountPair (zeros a + zeros b) (ones a + ones b)

instance (Monoid CountPair) where
  mempty = CountPair 0 0

bitToCountPair 0 = CountPair 1 0
bitToCountPair 1 = CountPair 0 1
bitToCountPair d = error $ "Invalid countPair digit " ++ show d

charToBit '0' = 0
charToBit '1' = 1
charToBit s = error $ "Invalid charToBit digit" ++ [s]

mostCommonBit :: CountPair -> Int
mostCommonBit pair
  | zeros pair > ones pair = 0
  | otherwise              = 1

leastCommonBit :: CountPair -> Int
leastCommonBit pair
  | zeros pair <= ones pair = 0
  | otherwise               = 1



toInt :: [Int] -> Int
toInt = foldl' (.|.) 0 . map (\(place, _) -> bit place) . filter (\(_, bit) -> bit == 1) . zip [11,10..]

omega :: [CountPair] -> Int
omega = toInt . map mostCommonBit

part1 = do counts <- foldr1 (zipWith (<>)) . map (map bitToCountPair) <$> input []
           let o = omega counts
           let e = 0x0fff .&. (complement o)
           putStrLn $ show (o, e, o * e)

searchFor criteria nums0 = go $ zip nums0 nums0
  where
    go [] = error "we have gone too far"
    go [(_, bits)] = toInt bits
    go nums = let counts = foldl' (<>) mempty . map (bitToCountPair . head . fst) $ nums
                  digitToMatch = criteria counts
                  matchingRows = filter ((== digitToMatch) . head . fst) nums
               in go $ map (\(remainingBits, allBits) -> (tail remainingBits, allBits)) matchingRows

part2 args = do nums <- input args
                let oxygen = searchFor mostCommonBit nums
                let co2 = searchFor leastCommonBit nums
                putStrLn $ show (oxygen, co2, oxygen * co2)

day3 Part1 _ = part1
day3 Part2 args = part2 args
