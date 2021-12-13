module Day13 (day13) where

import Control.Monad (forM_)
import Data.Foldable (maximum, foldl')
import Data.List.Split (wordsBy)
import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

type Point = (Int, Int)
data Axis = X | Y
  deriving (Show)
type Fold = (Axis, Int)

parseAxis "x" = X
parseAxis "y" = Y
parseAxis s = error $ "not an axis: '" ++ s ++ "'"

parseInput :: String -> (Set Point,[Fold])
parseInput input = let [pointLines, foldLines] = wordsBy (== "") . lines $ input
                       points = map (\line -> let [x,y] = wordsBy (== ',') line in (read x, read y)) pointLines
                       folds = map ((\line -> let [axis,pos] = wordsBy (== '=') line in (parseAxis axis,read pos)) . drop 11) $ foldLines
                    in (Set.fromList points, folds)

bendPoint :: Fold -> Point -> Point
bendPoint (X, foldPoint) (x, y) | x < foldPoint = (x, y)
                                | x > foldPoint = (foldPoint - (x - foldPoint), y)
                                | otherwise = error "make smaller I guess"
bendPoint (Y, foldPoint) (x, y) | y < foldPoint = (x, y)
                                | y > foldPoint = (x, foldPoint - (y - foldPoint))
                                | otherwise = error "make smaller I guess"


bendSheet :: Set Point -> Fold -> Set Point
bendSheet = flip (Set.map . bendPoint)

bendAll :: (Set Point,[Fold]) -> Set Point
bendAll (points, folds) = foldl' bendSheet points folds

bendFirst :: (Set Point,[Fold]) -> Set Point
bendFirst (points, folds) = bendSheet points (head folds)

part1 :: (Set Point,[Fold]) -> IO ()
part1 = putStrLn . show . Set.size . bendFirst

visualisePoints :: Set Point -> IO ()
visualisePoints points = let maxX = maximum (fst <$> Set.toList points)
                             maxY = maximum (snd <$> Set.toList points)
                          in forM_ [0..maxY] $ \y -> do
                               putStrLn $ fmap (\x -> if Set.member (x,y) points then 'X' else ' ') [0..maxX]

part2 :: (Set Point,[Fold]) -> IO ()
part2 = visualisePoints . bendAll

day13 part args = do let filename = case args of
                                      [] -> "inputs/day13"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
