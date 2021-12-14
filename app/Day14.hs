module Day14 (day14) where

import Data.List.Split (wordsBy)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Part (Part (Part1, Part2))

parseInput :: String -> (String,Map (Char, Char) Char)
parseInput input = let [initialLines, ruleLines] = wordsBy (== "") . lines $ input
                       [initial] = initialLines
                       rules = Map.fromList . map (\line -> let [[a,b],_,[c]] = wordsBy (== ' ') line in ((a,b),c)) $ ruleLines
                    in (initial, rules)

applyRules rules = go
  where
    go (a:b:s) = case Map.lookup (a,b) rules of
                   Just c -> a:c:(go $ b:s)
                   Nothing -> a:(go $ b:s)
    go s = s

countChars :: String -> Map Char Integer
countChars cs = foldl' (\counts c -> Map.alter (Just . (+ 1) . fromMaybe 0) c counts) Map.empty cs

toPairs :: String -> Map (Char, Char) Integer
toPairs s = Map.fromListWith (+) $ zipWith (\a b -> ((a, b), 1)) s (drop 1 s)

applyRulesPairs :: Map (Char, Char) Char -> Map (Char, Char) Integer -> Map (Char, Char) Integer
applyRulesPairs rules = Map.unionsWith (+) . concat . fmap go . Map.assocs
  where
    go ((a,b), count) = case Map.lookup (a,b) rules of
      Just c -> [Map.singleton (a, c) count, Map.singleton (c, b) count]
      Nothing -> [Map.singleton (a, b) count]

-- how do we map from a bunch of counts of how many times a pair of characters (a,b) appears in a string?
-- well, we know that each time a character appears in the string before a character, it also appears
-- after a character, unless it's at the start of the string. So we can just add up all the times a
-- character appears second in a pair, and then add one to whatever starts the string off to account for
-- that one special case
pairsFreqs :: String -> Map (Char, Char) Integer -> Map Char Integer
pairsFreqs template = Map.alter (fmap (+1)) (head template) . Map.mapKeysWith (+) snd

part1 :: (String,Map (Char, Char) Char) -> IO ()
part1 (template, rules) = do let freqs = Map.elems . countChars . head . drop 10 . iterate (applyRules rules) $ template
                             putStrLn . show $ freqs
                             putStrLn . show $ maximum freqs - minimum freqs
                             let pairWise = pairsFreqs template . head . drop 10 . iterate (applyRulesPairs rules) . toPairs $ template
                             putStrLn . show $ pairWise
                             putStrLn . show $ maximum pairWise - minimum pairWise

part2 :: (String,Map (Char, Char) Char) -> IO ()
part2 (template, rules) = do let pairWise = pairsFreqs template . head . drop 40 . iterate (applyRulesPairs rules) . toPairs $ template
                             putStrLn . show $ pairWise
                             putStrLn . show $ maximum pairWise - minimum pairWise

day14 part args = do let filename = case args of
                                      [] -> "inputs/day14"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
