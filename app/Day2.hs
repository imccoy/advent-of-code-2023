{-# LANGUAGE TemplateHaskell #-}
module Day2 (day2) where

import Data.Foldable (foldl')
import Control.Lens
import Control.Lens.TH

import Part (Part (Part1, Part2))

newtype Depth = Depth { _depth :: Int }
  deriving (Show)
makeLenses ''Depth

newtype Horizontal = Horizontal { _horizontal :: Int }
  deriving (Show)
makeLenses ''Horizontal

newtype Aim = Aim { _aim :: Int }
  deriving (Show)
makeLenses ''Aim

data Position = Position { _posHorizontal :: !Horizontal, _posDepth :: !Depth, _posAim ::  !Aim }
  deriving (Show)
makeLenses ''Position

data Command = Forward !Int | Down !Int | Up !Int


applyCommand1 (Forward distance) = posHorizontal . horizontal %~ (+ distance)
applyCommand1 (Down distance) = posDepth . depth +~ distance
applyCommand1 (Up distance) = posDepth . depth -~ distance

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


part1 = run (flip applyCommand1)

applyCommand2 pos (Forward distance) =
  (posHorizontal . horizontal +~ distance) $
  (posDepth . depth +~ distance * (pos ^. (posAim . aim))) $
  pos
applyCommand2 pos (Down distance) = posAim . aim +~ distance $ pos
applyCommand2 pos (Up distance) = posAim . aim -~ distance $ pos



part2 = run applyCommand2

day2 Part1 _ = part1
day2 Part2 _ = part2
