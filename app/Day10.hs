{-# LANGUAGE BangPatterns #-}
module Day10 (day10) where

import Data.List (sort)
import Data.Maybe (catMaybes)

import Part (Part (Part1, Part2))

isOpening '[' = True
isOpening '{' = True
isOpening '(' = True
isOpening '<' = True
isOpening _ = False

closingFor '[' = ']'
closingFor '{' = '}'
closingFor '(' = ')'
closingFor '<' = '>'
closingFor _ = error "no closing"

matchingPair '[' ']' = True
matchingPair '{' '}' = True
matchingPair '(' ')' = True
matchingPair '<' '>' = True
matchingPair _ _ = False

data ParseResult a = Ok a | Err String | Fin String
  deriving (Show)

firstIllegal cs = case parse cs of
                        Err (c:_) -> Just c
                        _ -> Nothing

parse s = case parse1 s of
            Ok s' -> parse s'
            r -> r

parse1 (startChar:c:cs) | matchingPair startChar c = Ok cs
                        | isOpening c = case parse1 $ c:cs of
                                          Ok cs' -> parse1 $ startChar:cs'
                                          Fin cs' -> Fin $ startChar:cs'
                                          r -> r
                        | otherwise = Err $ c:cs
parse1 [c] = Fin [c]
parse1 [] = Fin []

incompleteness cs = case parse cs of
                      Fin cs -> Just cs
                      _ -> Nothing

errScore ')' = 3
errScore ']' = 57
errScore '}' = 1197
errScore '>' = 25137

completionCharScore ')' = 1
completionCharScore ']' = 2
completionCharScore '}' = 3
completionCharScore '>' = 4

completionScore = go 0
  where
    go !s [] = s
    go !s (c:cs) = go (s * 5 + completionCharScore c) cs

part1 :: Parsed -> IO ()
part1 navigations = putStrLn . show . sum . catMaybes . fmap (fmap errScore . firstIllegal) $ navigations

part2 :: Parsed -> IO ()
part2 navigations = let scores = sort . fmap (completionScore . fmap closingFor . reverse) . catMaybes . fmap incompleteness $ navigations
                     in putStrLn . show $ scores !! (length scores `div` 2)

type Parsed = [String]

parseInput :: String -> Parsed
parseInput = lines

day10 part args = do let filename = case args of
                                      [] -> "inputs/day10"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
