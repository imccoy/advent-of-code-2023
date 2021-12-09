{-# LANGUAGE BlockArguments #-}

module Day9 (day9) where

import Part (Part (Part1, Part2))

import Control.Monad (forM)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Vector (Vector, (!?), (!))
import qualified Data.Vector as Vector
import Data.Set (Set)
import qualified Data.Set as Set

input :: [String] -> IO (Vector (Vector Int))
input args = Vector.fromList . fmap (Vector.fromList . fmap (\c -> read [c])) . lines <$> readFile filename
  where filename = case args of
                     [] -> "inputs/day9"
                     [f] -> f

pointsWithAdjs :: Vector (Vector Int) -> [(Int, (Int, Int), [Int])]
pointsWithAdjs ps = [ ((ps ! x) ! y 
                      ,(x, y)
                      ,catMaybes [(ps !? (x-1)) >>= (!? y)
                                 ,(ps !? (x+1)) >>= (!? y)
                                 ,(ps !? x) >>= (!? (y-1))
                                 ,(ps !? x) >>= (!? (y+1))
                                 ])
                    | x <- [0..length ps - 1]
                    , y <- [0..length (ps ! x) - 1]
                    ]

lowPoints = filter (\(p, _, adjs) -> all (> p) adjs)

risk :: [(Int, a, [Int])] -> Int
risk = sum . map (\(d, _, _) -> d + 1) . lowPoints

neighbours ps (x, y) = catMaybes [if x - 1 >= 0              then Just (x-1, y) else Nothing
                                 ,if x + 1 < length ps       then Just (x+1, y) else Nothing
                                 ,if y - 1 >= 0              then Just (x, y-1) else Nothing
                                 ,if y + 1 < length (ps ! x) then Just (x, y+1) else Nothing
                                 ]

lookupPoint :: Vector (Vector Int) -> (Int, Int) -> Int
lookupPoint ps (x,y) = (ps ! x) ! y

basins :: Vector (Vector Int) -> [Set (Int, Int)]
basins ps = fmap (\(_, p, _) -> growBasin p Set.empty) . lowPoints . pointsWithAdjs $ ps
  where
    growBasin :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
    growBasin point basin
      | Set.member point basin = basin
      | otherwise = let here = lookupPoint ps point
                     in case filter ((< 9) . lookupPoint ps) (neighbours ps point) of
                          [] -> basin
                          ns -> foldr (\p b -> growBasin p b) (Set.insert point basin) ns

part1 args = putStrLn . show . risk . lowPoints . pointsWithAdjs =<< input args
part2 args = do terrain <- input args
                let bs = basins terrain
                forM [0..length terrain - 1] $ \x -> do
                  let cells = map (\y -> if any (Set.member (x,y)) bs then show $ lookupPoint terrain (x,y) else " ")
                                  [0..length (terrain ! x) - 1]
                  putStrLn $ concat cells

                putStrLn . show . product . take 3 . reverse . sort . fmap Set.size . basins $ terrain

day9 Part1 args = part1 args
day9 Part2 args = part2 args
