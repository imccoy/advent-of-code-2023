module Part where

data Part = Part1 | Part2
  deriving (Show)

getPart "1" = Just Part1
getPart "2" = Just Part2
getPart _ = Nothing


