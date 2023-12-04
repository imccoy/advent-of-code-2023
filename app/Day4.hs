module Day4 (day4) where
import Part (Part (Part1, Part2))

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec

type Parsed = [(Int, ([Int], [Int]))]

parseGame = do void $ string "Card"
               void $ spaces
               n <- read <$> many1 digit
               void $ char ':'
               void $ spaces
               winningNumbers <- manyTill (do n <- many1 digit
                                              void $ spaces
                                              pure $ read n
                                          ) (try (char '|'))
               void $ spaces
               yourNumbers <- fmap read <$> sepBy (many1 digit) spaces
               pure (n, (winningNumbers, yourNumbers))

parseInput :: String -> Parsed
parseInput = fmap parseLine . lines
parseLine = either (error . show) id . runParser parseGame () "none" 

scoreCard (_, (winningNumbers, yourNumbers)) = case length $ List.intersect winningNumbers yourNumbers of
                                                 0 -> 0
                                                 n -> 2 ^ (n - 1)

part1 :: Parsed -> IO ()
part1 = putStrLn . show . sum . map scoreCard

scoreRecursive :: Map.Map Int Int -> Int -> [(Int,([Int],[Int]))] -> Int
scoreRecursive _ score [] = score
scoreRecursive cardCounts score ((number, (winningNumbers, yourNumbers)):rest)
  = let matches = List.intersect winningNumbers yourNumbers
        copiesHere = fromJust $ Map.lookup number cardCounts
        cardsWon = [(number+1)..(number+length matches)]
        cardCounts' = foldr (\cardNumber counts -> Map.adjust (+ copiesHere) cardNumber counts) cardCounts cardsWon
     in scoreRecursive cardCounts' (score + (length cardsWon) * copiesHere) rest

part2 :: Parsed -> IO ()
part2 input = putStrLn $ show $ scoreRecursive (Map.fromList [(n, 1) | (n,_) <- input]) (length input) input
               

day4 part args = do let filename = case args of
                                     [] -> "inputs/day4"
                                     [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
