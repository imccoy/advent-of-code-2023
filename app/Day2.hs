module Day2 (day2) where

import Data.Foldable (foldl')

import Part (Part (Part1, Part2))

newtype Depth = Depth Int
  deriving (Show)

newtype Horizontal = Horizontal Int
  deriving (Show)

newtype Aim = Aim Int
  deriving (Show)

data Position = Position !Horizontal !Depth !Aim
  deriving (Show)

data Command = Forward !Int | Down !Int | Up !Int


applyCommand1 (Position (Horizontal h) d a) (Forward distance) = Position (Horizontal $ h + distance) d a
applyCommand1 (Position h (Depth d) a) (Down distance) = Position h (Depth $ d + distance) a
applyCommand1 (Position h (Depth d) a) (Up distance) = Position h (Depth $ d - distance) a

readCommand line = case words line of
                     ["forward",amount] -> Forward (read amount)
                     ["down", amount] -> Down (read amount)
                     ["up", amount] -> Up (read amount)
                     _ -> error $ "Unparsable line " ++ line

input :: IO String
input = readFile "inputs/day2"

startPosition = Position (Horizontal 0) (Depth 0) (Aim 0)

navigate :: (Position -> Command -> Position) -> [Command] -> Position
navigate applyCommand = foldl' applyCommand startPosition


run applyCommand = do
  result <- navigate applyCommand . fmap readCommand . lines <$> input
  putStrLn . show $ result
  let (Position (Horizontal h) (Depth d) _) = result
  putStrLn . show $ h * d


part1 = run applyCommand1

applyCommand2 (Position (Horizontal h) (Depth d) (Aim a)) (Forward distance) =
  Position
    (Horizontal $ h + distance)
    (Depth $ d + a * distance)
    (Aim a)
applyCommand2 (Position h d (Aim a)) (Down distance) = Position h d (Aim $ a + distance)
applyCommand2 (Position h d (Aim a)) (Up distance) = Position h d (Aim $ a - distance)



part2 = run applyCommand2

day2 Part1 _ = part1
day2 Part2 _ = part2
