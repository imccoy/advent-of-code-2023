{-# LANGUAGE LambdaCase, DeriveFunctor, BangPatterns #-}
module Day3 where

import Control.Monad (forM)
import Data.Bits
import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Part (Part (Part1, Part2))
import Day3.Common
import qualified Day3.Vectory as Vectory

input :: [String] -> IO [[Bit]]
input args = map (map charToBit) . lines <$> readFile filename
  where filename = case args of
                     [] -> "inputs/day3"
                     [f] -> f
                     _ -> error "too many arguments"


part1 = do counts <- foldr1 (zipWith (<>)) . map (map bitToCountPair) <$> input []
           let gamma = map mostCommonBit counts
           let epsilon = map invertBit gamma
           let g = toInt gamma
           let e = toInt epsilon
           putStrLn $ show (g, e, g * e)

searchFor criteria nums0 = go $ zip nums0 nums0
  where
    go [] = error "we have gone too far"
    go [(_, bits)] = toInt bits
    go nums = let remainingLeadingDigits = map (head . fst) $ nums
                  counts = foldl' (<>) mempty . map bitToCountPair $ remainingLeadingDigits
                  digitToMatch = criteria counts
                  matchingRows = filter ((== digitToMatch) . head . fst) nums
               in go $ map (\(remainingBits, allBits) -> (tail remainingBits, allBits)) matchingRows



data SearchEntry a = SearchHere a | SearchSkip !Int
  deriving (Functor)

searchEntryMaybe (SearchHere a) = Just a
searchEntryMaybe (SearchSkip _) = Nothing

searchFor' :: (CountPair -> Bit) -> [[Bit]] -> Int
searchFor' criteria nums0 = toInt . go . ((SearchSkip 0):) . map SearchHere $ nums0
  where
    go :: [SearchEntry [Bit]] -> [Bit]
    go [] = error "we have gone too far"
    go [SearchSkip _] = error "we have gone too far"
    go [SearchSkip n, SearchHere _, SearchSkip _] = head . drop n $ nums0
    go nums = let splits = fmap (\case
                                   (SearchSkip n) -> SearchSkip n
                                   (SearchHere []) -> error "empty SearchHere"
                                   (SearchHere (x:xs)) -> SearchHere (x, xs)
                                )
                                nums
                  leadingDigits = mapMaybe (fmap fst . searchEntryMaybe) splits
                  counts = foldl' (<>) mempty . map bitToCountPair $ leadingDigits
                  digitToMatch = criteria counts
               in go . mergeSkips . map (\case
                                           SearchHere (x, xs) -> if x == digitToMatch
                                                                   then SearchHere xs
                                                                   else SearchSkip 1
                                           SearchSkip n -> SearchSkip n
                                        ) $ splits
    mergeSkips :: [SearchEntry a] -> [SearchEntry a]
    mergeSkips ((SearchSkip n):((SearchSkip m):rest)) = mergeSkips $ (SearchSkip (n+m)):(mergeSkips rest)
    mergeSkips (here:rest) = here:(mergeSkips rest)
    mergeSkips [] = []

searchFor'' criteria nums0 = go $ zip nums0 (repeat 0)
  where
    go :: [([Bit], Int)] -> Int
    go [] = error "we have gone too far"
    go [(bits, n)] = foldl' (\n b -> 2 * n + bitToInt b) n bits
    go nums = let (leading0s, leading1s) = foldr (\(bit:nums', n) (leading0s, leading1s) -> 
                                                    let !n' = n * 2 + bitToInt bit
                                                     in case bit of
                                                          Bit0 -> ((nums', n'):leading0s, leading1s)
                                                          Bit1 -> (leading0s, (nums', n'):leading1s)
                                                 ) ([],[]) nums
               in case criteria $ CountPair (length leading0s) (length leading1s) of
                    Bit0 -> go leading0s
                    Bit1 -> go leading1s


part2 args = do nums <- input args
                forM [searchFor, searchFor', searchFor''] $ \searchFor -> do
                  let oxygen = searchFor mostCommonBit nums
                  let co2 = searchFor leastCommonBit nums
                  putStrLn $ show (oxygen, co2, oxygen * co2)

day3 Part1 _ = do part1
                  Vectory.part1
day3 Part2 args = do part2 args
                     Vectory.part2 args
