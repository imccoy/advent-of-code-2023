{-# LANGUAGE BlockArguments, TupleSections #-}

module Day9.Funk where

import Part (Part (Part1, Part2))

import Control.Monad (forM)
import Data.List (sort)
import Data.Maybe (catMaybes, fromJust)
import Data.Vector (Vector, (!?), (!))
import qualified Data.Vector as Vector
import Data.Set (Set)
import qualified Data.Set as Set

input :: [String] -> IO [[Int]]
input args = fmap (fmap (\c -> read [c])) . lines <$> readFile filename
  where filename = case args of
                     [] -> "inputs/day9"
                     [f] -> f

data Cell = Cell { left :: Maybe Cell, right :: Maybe Cell, up :: Maybe Cell, down :: Maybe Cell, depth :: Int }


-- here we do some goofy knot-tying to make a self-referential grid structure
makeCells :: [[Int]] -> Cell
makeCells terrain = fromJust $ makeRows terrain Nothing
  where makeRows :: [[Int]] -> Maybe Cell -> Maybe Cell
        makeRows (t:ts) prevRow = let thisRow = makeRow t prevRow nextRow Nothing
                                      nextRow = makeRows ts thisRow
                                   in thisRow
        makeRows [] _ = Nothing
        makeRow (d:ds) p n l = let thisCell = Cell { left = l
                                                   , right = nextCell
                                                   , up = p
                                                   , down = n
                                                   , depth = d
                                         }
                                   nextCell = makeRow ds (right =<< p) (right =<< n) (Just thisCell)
                                in Just thisCell
        makeRow [] _ _ _ = Nothing

cellList :: Cell -> [Cell]
cellList = rows . Just
  where rows Nothing = []
        rows (Just r) = cols (Just r) ++ rows (down r)
        cols Nothing = []
        cols (Just c) = c:(cols (right c))


lowPoints = filter (\cell -> all ((> depth cell) . depth) $ catMaybes [left cell, right cell, up cell, down cell]) . cellList

risk :: Cell -> Int
risk = sum . map (\cell -> depth cell + 1) . lowPoints

neighbours :: Cell -> (Int, Int) -> [((Int,Int),Cell)]
neighbours p (x, y) = catMaybes [((x+1,y),) <$> up p
                                ,((x-1,y),) <$> down p
                                ,((x,y+1),) <$> right p
                                ,((x,y-1),) <$> left p
                                ]

basinSizes :: [Cell] -> [Int]
basinSizes ps = Set.size . growBasin Set.empty (0,0) <$> ps
  where
    growBasin :: Set (Int, Int) -> (Int, Int) -> Cell -> Set (Int, Int)
    growBasin basin point cell
      | Set.member point basin = basin
      | depth cell == 9 = basin
      | otherwise = foldr (\(p', cell') b' -> growBasin b' p' cell') (Set.insert point basin) $ neighbours cell point
 
part1 args = do
  putStrLn . show . risk . makeCells =<< input args

part2 args = do
  putStrLn . show . product . take 3 . reverse . sort . basinSizes . lowPoints . makeCells =<< input args

day9 Part1 args = part1 args
-- day9 Part2 args = part2 args
