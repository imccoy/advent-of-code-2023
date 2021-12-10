{-# LANGUAGE BlockArguments, TupleSections #-}

module Day9.Funkr where

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

newtype CellId = CellId Int
  deriving (Show, Eq, Ord)

data Cell = Cell { left :: Maybe Cell, right :: Maybe Cell, up :: Maybe Cell, down :: Maybe Cell, depth :: Int, cellId :: CellId }


-- here we do some goofy knot-tying to make a self-referential grid structure
makeCells :: [[Int]] -> Cell
makeCells terrain = fromJust $ makeRows terrain Nothing 0
  where makeRows :: [[Int]] -> Maybe Cell -> Int -> Maybe Cell
        makeRows (t:ts) prevRow currId = let (thisRow, thisRowLastId) = makeRow t prevRow nextRow Nothing currId
                                             nextRow = makeRows ts thisRow thisRowLastId
                                          in thisRow
        makeRows [] _ currId = Nothing
        makeRow (d:ds) p n l cId = let thisCell = Cell { left = l
                                                       , right = nextCell
                                                       , up = p
                                                       , down = n
                                                       , depth = d
                                                       , cellId = (CellId cId)
                                             }
                                       (nextCell, lastIdInRow) = makeRow ds (right =<< p) (right =<< n) (Just thisCell) (cId+1)
                                    in (Just thisCell, lastIdInRow)
        makeRow [] _ _ _ cId = (Nothing, cId)

cellList :: Cell -> [Cell]
cellList = rows . Just
  where rows Nothing = []
        rows (Just r) = cols (Just r) ++ rows (down r)
        cols Nothing = []
        cols (Just c) = c:(cols (right c))


lowPoints = filter (\cell -> all ((> depth cell) . depth) . neighbours $ cell) . cellList

risk :: Cell -> Int
risk = sum . map (\cell -> depth cell + 1) . lowPoints

neighbours :: Cell -> [Cell]
neighbours p = catMaybes [up p
                         ,down p
                         ,right p
                         ,left p
                         ]

basinSizes :: [Cell] -> [Int]
basinSizes ps = Set.size . growBasin Set.empty <$> ps
  where
    growBasin :: Set CellId -> Cell -> Set CellId
    growBasin basin cell
      | Set.member (cellId cell) basin = basin
      | depth cell == 9 = basin
      | otherwise = foldr (\cell' b' -> growBasin b' cell') (Set.insert (cellId cell) basin) $ neighbours cell
 
part1 args = do
  putStrLn . show . risk . makeCells =<< input args

part2 args = do
  putStrLn . show . product . take 3 . reverse . sort . basinSizes . lowPoints . makeCells =<< input args

day9 Part1 args = part1 args
-- day9 Part2 args = part2 args
