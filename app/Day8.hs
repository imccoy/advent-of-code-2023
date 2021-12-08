module Day8 (day8) where

import Part (Part (Part1, Part2))

import qualified Day8.Lol as Lol

import Data.List (partition, permutations)
import Data.List.Split (wordsBy)
import Data.Maybe (isJust, listToMaybe)

data Segment = SegA | SegB | SegC | SegD | SegE | SegF | SegG
  deriving (Show, Enum, Eq)

fromChar 'a' = SegA
fromChar 'b' = SegB
fromChar 'c' = SegC
fromChar 'd' = SegD
fromChar 'e' = SegE
fromChar 'f' = SegF
fromChar 'g' = SegG

digit0 = [SegA, SegB, SegC, SegE, SegF, SegG]
digit1 = [SegC, SegF]
digit2 = [SegA, SegC, SegD, SegE, SegG]
digit3 = [SegA, SegC, SegD, SegF, SegG]
digit4 = [SegB, SegC, SegD, SegF]
digit5 = [SegA, SegB, SegD, SegF, SegG]
digit6 = [SegA, SegB, SegD, SegE, SegF, SegG]
digit7 = [SegA, SegC, SegF]
digit8 = [SegA, SegB, SegC, SegD, SegE, SegF, SegG]
digit9 = [SegA, SegB, SegC, SegD, SegF, SegG]

digits = [digit0
         ,digit1
         ,digit2
         ,digit3
         ,digit4
         ,digit5
         ,digit6
         ,digit7
         ,digit8
         ,digit9
         ]

type Display = ([[Segment]], [[Segment]])

parseDisplay display = let [readings, signals] = wordsBy (== '|') display
                        in (map fromChar <$> words readings, map fromChar <$> words signals)

input :: IO [Display]
input = fmap parseDisplay . lines <$> readFile "inputs/day8"

countSignalsWithLength ls = length . (filter . flip elem $ ls) . map length . concat . map snd

allMappings :: [[(Segment, Segment)]]
allMappings = zip [SegA .. SegG] <$> permutations [SegA .. SegG]

findValidMapping readings = case filter (\m -> all (isValidMapping m) readings) $ allMappings of
                              [entry] -> entry
                              [] -> error $ "no mapping for " ++ show readings
                              _ -> error $ "too many mappings for " ++ show readings

findDigit :: [Segment] -> Maybe Int
findDigit segments = go segments $ zip [0..] digits
  where go [] digits = listToMaybe . fmap fst . filter ((== 0) . length . snd) $ digits
        go (signal:signals) digits = go signals [ (n, otherSegs)
                                                | (n, segments) <- digits
                                                , let (thisSeg, otherSegs) = partition (== signal) segments
                                                , length thisSeg == 1
                                                ]
        go _ [] = Nothing


definitelyFindDigit :: [Segment] -> Int
definitelyFindDigit segments = case findDigit segments of
                                 Just x -> x
                                 Nothing -> error $ "No digit for " ++ show segments


applyMapping :: [(Segment, Segment)] -> [Segment] -> [Segment]
applyMapping mapping = map $ \x -> case lookup x mapping of
                                     Just mapped -> mapped
                                     Nothing -> error $ "No mapping for " ++ show x

isValidMapping mapping reading = isJust . findDigit . applyMapping mapping $ reading

figureOutput :: Display -> Int
figureOutput (readings, signals) = let mapping = findValidMapping readings
                                       [a,b,c,d] = (definitelyFindDigit . applyMapping mapping) <$> signals
                                    in a * 1000 + b * 100 + c * 10 + d

figureOutputs :: [Display] -> Int
figureOutputs = sum . map figureOutput

part1 = putStrLn . show . countSignalsWithLength (map length [digit1, digit4, digit7, digit8]) =<< input
part2 = putStrLn . show . figureOutputs =<< input

day8 Part1 _ = do part1
                  Lol.part1
day8 Part2 _ = do part2
                  Lol.part2
