module Day2 (day2) where

import Data.Foldable (foldl')

import Part (Part (Part1, Part2))

newtype Depth = Depth Int
  deriving (Show)

newtype Horizontal = Horizontal Int
  deriving (Show)

data Position = Position !Horizontal !Depth
  deriving (Show)

data Command = Forward !Int | Down !Int | Up !Int

applyCommand (Position (Horizontal h) d) (Forward distance) = Position (Horizontal $ h + distance) d
applyCommand (Position h (Depth d)) (Down distance) = Position h (Depth $ d + distance)
applyCommand (Position h (Depth d)) (Up distance) = Position h (Depth $ d - distance)

readCommand line = case words line of
                     ["forward",amount] -> Forward (read amount)
                     ["down", amount] -> Down (read amount)
                     ["up", amount] -> Up (read amount)

input :: IO String
input = readFile "inputs/day2"

startPosition = Position (Horizontal 0) (Depth 0)

navigate :: [Command] -> Position
navigate = foldl' applyCommand startPosition

part1 = do result <- navigate . fmap readCommand . lines <$> input
           putStrLn . show $ result
           let (Position (Horizontal h) (Depth d)) = result
           putStrLn . show $ h * d
part2 = putStrLn "part2"

day2 Part1 _ = part1
day2 Part2 _ = part2
