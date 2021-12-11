module Day11 (day11) where

import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

neighbours :: Vector (Vector Int) -> (Int, Int) -> [(Int, Int)]
neighbours ps (x, y) = catMaybes [ adj (-1) (-1)
                                 , adj (-1) 0
                                 , adj (-1) 1
                                 , adj 0 1
                                 , adj 1 1
                                 , adj 1 0
                                 , adj 1 (-1)
                                 , adj 0 (-1)
                                 ]
  where adj xd yd | x + xd < 0 || x + xd >= length ps              = Nothing
                  | y + yd < 0 || y + yd >= length (ps ! (x + xd)) = Nothing
                  | otherwise                                      = Just (x + xd, y + yd)

propagateFlashes :: Vector (Vector Int) -> [(Int, Int)] -> Set (Int, Int) -> (Set (Int, Int), Vector (Vector Int))
propagateFlashes octos [] already = (already, octos)
propagateFlashes octos locs already = let flashNow = filter (\(x, y) -> (octos ! x) ! y > 9 && not (Set.member (x,y) already)) locs
                                          octos' = foldr (\(x,y) octos -> octos // [(x, octos ! x // [(y, ((octos ! x) ! y) + 1)])])
                                                         octos
                                                         (concat . fmap (neighbours octos) $ flashNow)
                                       in foldr (\flasherNow (already, octos) -> 
                                                   propagateFlashes
                                                     octos
                                                     (neighbours octos flasherNow)
                                                     already
                                                )
                                                (already <> Set.fromList flashNow, octos')
                                                flashNow
                                  
                                  

step :: Vector (Vector Int) -> (Int, Vector (Vector Int))
step octos = let afterIncr = fmap (fmap (+ 1)) octos
                 (flashed, afterFlashes) = propagateFlashes afterIncr [(x, y) | x <- [0..9], y <- [0..9]] Set.empty
              in (Set.size flashed, Vec.imap (\x -> Vec.imap (\y octo -> if Set.member (x,y) flashed then 0 else octo)) afterFlashes)

steps :: Int -> Vector (Vector Int) -> IO Int
steps 0 _ = pure 0
steps n octos = do forM octos $ \row -> do
                     putStrLn . concat . fmap show . Vec.toList $ row
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

parseInput :: String -> Vector (Vector Int)
parseInput = Vec.fromList . fmap (Vec.fromList . fmap (read . pure)) . lines

part1 :: Vector (Vector Int) -> IO ()
part1 octos = putStrLn . show =<< steps 100 octos

part2 :: Vector (Vector Int) -> IO ()
part2 octos = putStrLn . show . firstWithFlashes (sum . fmap Vec.length $ octos) $ octos

day11 part args = do let filename = case args of
                                      [] -> "inputs/day11"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
