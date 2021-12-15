{-# LANGUAGE TupleSections #-}
module Day15 (day15) where

import Control.Monad (forM_)
import Data.Foldable (maximum)
import Data.List (nub)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)

import Part (Part (Part1, Part2))

parseInput :: String -> Map (Int, Int) Int
parseInput = Map.fromList . concat . fmap (\(x, ys) -> fmap (\(y, o) -> ((x, y), read [o])) ys) . zip [0..] . fmap (zip [0..]) . lines

neighbours :: (Int, Int) -> Map (Int, Int) v -> [((Int, Int), v)]
neighbours (x, y) risks = catMaybes [ adj (-1) 0
                                    , adj 0 1
                                    , adj 1 0
                                    , adj 0 (-1)
                                    ]
  where adj xd yd = let p = (x + xd, y + yd)
                     in (p,) <$> Map.lookup p risks

lowestRiskFrom :: Map (Int, Int) Int -> Map (Int, Int) Int -> [(Int, Int)] -> Map (Int, Int) Int
lowestRiskFrom risks pathCosts [] = pathCosts
lowestRiskFrom risks pathCosts ps =
  let includeSite fromPoint toPoint cost = let newCost = pathCosts ! fromPoint + cost
                                            in case Map.lookup toPoint pathCosts of
                                                 Nothing -> Just (toPoint, newCost)
                                                 Just currentBest -> if newCost < currentBest
                                                                       then Just (toPoint, newCost)
                                                                       else Nothing
      pointCosts = catMaybes . concat . fmap (\p -> fmap (\(n, leavingCost) -> includeSite p n leavingCost) $ neighbours p risks) $ ps
   in lowestRiskFrom risks (foldr (\(p, c) -> Map.insertWith min p c) pathCosts pointCosts) (nub $ fst <$> pointCosts)

lowestRiskFromOrigin risks = lowestRiskFrom risks (Map.singleton (0,0) 0) [(0, 0)]

bottomRight :: Map (Int,Int) v -> v
bottomRight = snd . Map.findMax

padLeft c n s = take (n - length s) (repeat c) ++ s

printRisks' width keys risks =
  forM_ [0..maximum (fst <$> keys)] $ \x ->
    putStrLn . unwords $ [padLeft ' ' width . fromMaybe "?"  $ show <$> risks !? (x,y) | y <- [0..maximum (snd <$> keys)]]

printRisks width risks = printRisks' width (Map.keys risks) risks

part1 :: Map (Int, Int) Int -> IO ()
part1 risks = do
  printRisks 0 risks
  putStrLn "===="
  putStrLn . show . neighbours (2, 0) $ risks
  putStrLn "===="

  let costs = lowestRiskFromOrigin risks
  printRisks' 3 (Map.keys risks) costs
  putStrLn . show . bottomRight . lowestRiskFromOrigin $ risks

moduloIncrement :: Int -> Int -> Int
moduloIncrement n = (+ 1) . (`mod` 9) . (+ (-1)) . (+ n)

growRight :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int
growRight count risks = Map.unions . (risks:) $ shiftRight <$> [1..count] <*> pure risks
  where originalWidth = (+1) . maximum $ snd <$> Map.keys risks
        shiftRight :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int
        shiftRight n = Map.mapKeys (\(x,y) -> (x, y + n * originalWidth)) . fmap (moduloIncrement n)

growDown :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int
growDown count risks = Map.unions . (risks:) $ shiftDown <$> [1..count] <*> pure risks
  where originalHeight = (+1) . maximum $ fst <$> Map.keys risks
        shiftDown n = Map.mapKeys (\(x,y) -> (x + n * originalHeight, y)) . fmap (moduloIncrement n)


growMap = growDown 4 . growRight 4

part2 :: Map (Int, Int) Int -> IO ()
part2 risks = do
  putStrLn . show $ moduloIncrement 1 <$> [1..10]
  printRisks 0 $ growMap (Map.singleton (0,0) 8)
  printRisks 0 $ growMap (Map.fromList [((0,0),8),((0,1),8),((1,0),8),((1,1),8)])
  putStrLn . show . bottomRight . lowestRiskFromOrigin . growMap $ risks

day15 part args = do let filename = case args of
                                      [] -> "inputs/day15"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
