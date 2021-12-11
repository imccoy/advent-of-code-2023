{-# LANGUAGE TupleSections #-}
module Day11 (day11) where

import Control.Monad (forM)
import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

maxX :: Int
maxX = 10
maxY :: Int
maxY = 10

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = catMaybes [ adj (-1) (-1)
                              , adj (-1) 0
                              , adj (-1) 1
                              , adj 0 1
                              , adj 1 1
                              , adj 1 0
                              , adj 1 (-1)
                              , adj 0 (-1)
                              ]
  where adj xd yd | x + xd < 0 || x + xd >= maxX = Nothing
                  | y + yd < 0 || y + yd >= maxY = Nothing
                  | otherwise                    = Just (x + xd, y + yd)

propagateFlashes :: Map (Int,Int) Int -> [(Int, Int)] -> Set (Int, Int) -> (Set (Int, Int), Map (Int, Int) Int)
propagateFlashes octos [] already = (already, octos)
propagateFlashes octos locs already = let flashNow = filter (\(x, y) -> (octos ! (x,y) > 9 && not (Set.member (x,y) already))) locs
                                          octos' = foldr (\(x,y) octos -> Map.update (Just . (+1)) (x,y) octos)
                                                         octos
                                                         (concat . fmap neighbours $ flashNow)
                                       in foldr (\flasherNow (already, octos) -> 
                                                   propagateFlashes
                                                     octos
                                                     (neighbours flasherNow)
                                                     already
                                                )
                                                (already <> Set.fromList flashNow, octos')
                                                flashNow
                                  
                                  

step :: Map (Int, Int) Int -> (Int, Map (Int, Int) Int)
step octos = let afterIncr = fmap (+ 1) octos
                 (flashed, afterFlashes) = propagateFlashes afterIncr [(x, y) | x <- [0..9], y <- [0..9]] Set.empty
              in (Set.size flashed, foldr (\k m -> Map.insert k 0 m) afterFlashes flashed)

steps :: Int -> Map (Int, Int) Int -> IO Int
steps 0 _ = pure 0
steps n octos = do forM [0..(maxX-1)] $ \row -> do
                     putStrLn . concat $ fmap (\col -> show $ octos ! (row, col)) [0..(maxY-1)]
                   let (flashCount, next) = step octos
                   putStrLn $ "============== " ++ show flashCount
                   flashCount' <- steps (n - 1) next
                   pure $ flashCount + flashCount'

firstWithFlashes requiredFlashes = go 1
  where go n octos = let (flashed, octos') = step octos
                      in if flashed == requiredFlashes
                           then n
                         else
                           go (n + 1) octos'

parseInput :: String -> Map (Int, Int) Int
parseInput = Map.fromList . concat . fmap (\(x, ys) -> fmap (\(y, o) -> ((x, y), read [o])) ys) . zip [0..] . fmap (zip [0..]) . lines

part1 :: Map (Int, Int) Int -> IO ()
part1 octos = do putStrLn . show . sort . Map.keys $ octos
                 putStrLn . show =<< steps 100 octos

part2 :: Map (Int, Int) Int -> IO ()
part2 octos = putStrLn . show . firstWithFlashes (Map.size octos) $ octos

day11 part args = do let filename = case args of
                                      [] -> "inputs/day11"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
