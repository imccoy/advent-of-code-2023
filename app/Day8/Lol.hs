{-# LANGUAGE DeriveFunctor #-}

module Day8.Lol where

import Part (Part (Part1, Part2))

import Data.List (partition, permutations)
import Data.List.Split (wordsBy)
import Data.Maybe (isJust, listToMaybe)

data Digit' v = Digit { segA :: v, segB :: v, segC :: v, segD :: v, segE :: v, segF :: v, segG :: v }
  deriving (Show, Eq, Functor)

type Digit = Digit' Bool

digitList (Digit { segA = a, segB = b, segC = c, segD = d, segE = e, segF = f, segG = g })
  = [a,b,c,d,e,f,g]

digitFromList [a,b,c,d,e,f,g] = Digit a b c d e f g

readSignal = go (Digit False False False False False False False)
  where
    go d "" = d
    go d ('a':signal) = go (d { segA = True }) signal
    go d ('b':signal) = go (d { segB = True }) signal
    go d ('c':signal) = go (d { segC = True }) signal
    go d ('d':signal) = go (d { segD = True }) signal
    go d ('e':signal) = go (d { segE = True }) signal
    go d ('f':signal) = go (d { segF = True }) signal
    go d ('g':signal) = go (d { segG = True }) signal

digit0 = readSignal "abcefg"
digit1 = readSignal "cf"
digit2 = readSignal "acdeg"
digit3 = readSignal "acdfg"
digit4 = readSignal "bcdf"
digit5 = readSignal "abdfg"
digit6 = readSignal "abdefg"
digit7 = readSignal "acf"
digit8 = readSignal "abcdefg"
digit9 = readSignal "abcdfg"

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

type Display = ([Digit], [Digit])

parseDisplay display = let [readings, signals] = wordsBy (== '|') display
                        in (readSignal <$> words readings, readSignal <$> words signals)

input :: IO [Display]
input = fmap parseDisplay . lines <$> readFile "inputs/day8"

digitLength = length . filter (== True) . digitList

countSignalsWithLength ls = length . (filter . flip elem $ ls) . map digitLength . concat . map snd

allMappings :: [Digit' (Digit -> Bool)]
allMappings = digitFromList <$> permutations [segA, segB, segC, segD, segE, segF, segG]

findValidMapping readings = case filter (\m -> all (isValidMapping m) readings) $ allMappings of
                              [entry] -> entry
                              [] -> error $ "no mapping for " ++ show readings
                              _ -> error $ "too many mappings for " ++ show readings

findDigit :: Digit -> Maybe Int
findDigit digit = lookup digit (zip digits [0..])

definitelyFindDigit :: Digit -> Int
definitelyFindDigit segments = case findDigit segments of
                                 Just x -> x
                                 Nothing -> error $ "No digit for " ++ show segments


applyMapping :: Digit' (Digit -> Bool) -> Digit -> Digit
applyMapping mapping digit = (\f -> f digit) <$> mapping

isValidMapping mapping reading = isJust . findDigit . applyMapping mapping $ reading

figureOutput :: Display -> Int
figureOutput (readings, signals) = let mapping = findValidMapping readings
                                       [a,b,c,d] = (definitelyFindDigit . applyMapping mapping) <$> signals
                                    in a * 1000 + b * 100 + c * 10 + d

figureOutputs :: [Display] -> Int
figureOutputs = sum . map figureOutput

part1 = putStrLn . show . countSignalsWithLength (map digitLength [digit1, digit4, digit7, digit8]) =<< input
part2 = putStrLn . show . figureOutputs =<< input

day8 Part1 _ = part1
day8 Part2 _ = part2
