module Day20 (day20) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Text.Parsec

import Part (Part (Part1, Part2))

boolFromChar '#' = True
boolFromChar '.' = False

newtype BackgroundColor = BackgroundColor Bool

parseInput :: String -> (Vector Bool, BackgroundColor, Map (Int, Int) Bool)
parseInput input = let (program:_:image) = lines input
                    in ( Vector.fromList (boolFromChar <$> program)
                       , BackgroundColor False
                       , Map.fromList . concat . fmap (\(row, ys) -> fmap (\(col, o) -> ((row, col), boolFromChar o)) ys) . zip [0..] . fmap (zip [0..]) $ image
                       )

imageBounds pixels = ((minimum . fmap fst . Map.keys $ pixels, maximum . fmap fst . Map.keys $ pixels)
                     ,(minimum . fmap snd . Map.keys $ pixels, maximum . fmap snd . Map.keys $ pixels)
                     )

intFromBools :: [Bool] -> Int
intFromBools = go 0
  where go n (True:bools) = go (n * 2 + 1) bools
        go n (False:bools) = go (n * 2) bools
        go n [] = n

enhance algorithm ((BackgroundColor backgroundColor), pixels) =
  ( BackgroundColor $ enhancedBackground
  , Map.fromList $ do row <- [(rowMin - 1)..(rowMax + 1)]
                      col <- [(colMin - 1)..(colMax + 1)]
                      pure ((row, col), algorithm ! calculatePixel row col)
  )
  where ((rowMin, rowMax), (colMin, colMax)) = imageBounds pixels
        calculatePixel row col = intFromBools . fmap lookupIndex $ [(row-1,col-1),(row-1,col),(row-1,col+1),
                                                                    (row,  col-1),(row,  col),(row  ,col+1),
                                                                    (row+1,col-1),(row+1,col),(row+1,col+1)]
        lookupIndex rowCol = fromMaybe backgroundColor . Map.lookup rowCol $ pixels
        enhancedBackground = algorithm ! (intFromBools $ replicate 9 backgroundColor)

part1 :: (Vector Bool, BackgroundColor, Map (Int, Int) Bool) -> IO ()
part1 (algorithm, background, pixels) = putStrLn . show . length . filter (== True) . Map.elems . snd . enhance algorithm . enhance algorithm $ (background, pixels)

part2 :: (Vector Bool, BackgroundColor, Map (Int, Int) Bool) -> IO ()
part2 (algorithm, background, pixels) = putStrLn . show . length . filter (== True) . Map.elems . snd . head . drop 50 . iterate (enhance algorithm) $ (background, pixels)

day20 part args = do let filename = case args of
                                      [] -> "inputs/day20"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
