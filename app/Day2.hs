module Day2 (day2) where
import Part (Part (Part1, Part2))

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Text.Parsec

data Game = Game Int [Round]
data Round = Round Int Int Int

type Parsed = [Game]

parseGame = do void $ string "Game "
               gameNumber <- read <$> many1 digit
               void $ string ": "
               Game gameNumber <$> sepBy parseRound (string "; ")

parseRound = do colours <- sepBy (do count <- read <$> many1 digit
                                     void $ char ' '
                                     colour <- string "red" <|> string "green" <|> string "blue"
                                     pure (colour, count)
                                 )
                                 (string ", ")
                let colourCount colour = fromMaybe 0 $ lookup colour colours
                pure $ Round (colourCount "red") (colourCount "green") (colourCount "blue")

parseInput :: String -> [Game]
parseInput = fmap parseLine . lines
parseLine = either (error . show) id . runParser parseGame () "none" 

part1 :: Parsed -> IO ()
part1 games = putStrLn $ show . sum . map (\(Game n _) -> n) . filter (\(Game _ rounds) -> all (\(Round r g b) -> r <= 12 && g <= 13 && b <= 14) rounds) $ games

powerContribution f rounds = case maximum (map f rounds) of
                               0 -> 1
                               n -> n

part2 :: Parsed -> IO ()
part2 games = putStrLn $ show . sum $ map (\(Game _ rounds) -> (powerContribution (\(Round r _ _) -> r) rounds) *
                                                               (powerContribution (\(Round _ g _) -> g) rounds) *
                                                               (powerContribution (\(Round _ _ b) -> b) rounds)
                                          ) games

day2 part args = do let filename = case args of
                                      [] -> "inputs/day2"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
