{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Day13 (day13) where
import Part (Part (Part1, Part2))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Data.List (transpose)
import Control.Monad (forM, mplus, forM_)
import Debug.Trace (trace)
import Control.Zipper (Top, (:>>), zipper, rightward, downward, focus, within, iwithin, fromWithin, idownward, rezip)
import Control.Lens ((&), _head, (%~), (<&>))
import Control.Lens.Getter (view)

type Parsed = [[[Bool]]]

data Reflection = Horizontal Int | Vertical Int
  deriving (Show, Eq)

parseInput :: String -> Parsed
parseInput = fmap parseChart . splitOn [""] . lines

parseChart :: [String] -> [[Bool]]
parseChart = map (map (/= '.'))

reflectionScore (Horizontal n) = 100 * (n+1)
reflectionScore (Vertical n) = n+1

part1 :: Parsed -> IO ()
part1 input = do nums <- forM input $ \chart -> do
                   let score = reflectionScore . findReflection $ chart
                   print score
                   pure score
                 print . sum $ nums

findReflection = head . findReflections

findReflections :: [[Bool]] -> [Reflection]
findReflections chart = fmap Horizontal (findHorizontalReflection chart) ++ fmap Vertical (findVerticalReflection chart)

findSets :: Int -> [[Int]] -> [[Bool]] -> [[Int]]
findSets _ sets [] = sets
findSets n sets (r:rows)
  | any (n `elem`) sets = findSets (n+1) sets rows
  | otherwise           = findSets (n+1)
                                   ((n:[ idx
                                       | (idx, r') <- zip [n+1..] rows
                                       , r == r'
                                       ]):sets)
                                   rows
--      |          |                 |          |
-- 0123456789    0123456789   0123456789    012345678 
-- 012345        012                98       8765
--   9876        543
--   len=10        len=10       len=10          len=9

findHorizontalReflection :: [[Bool]] -> [Int]
findHorizontalReflection rows = findMirrorPoints . findSets 0 [] $ rows
  where
    numRows = length rows
    findMirrorPoints :: [[Int]] -> [Int]
    findMirrorPoints sets = [ p
                            | p <- [0..numRows-2]
                            , all (mirroredAt p) sets
                            ]
    mirroredAt :: Int -> [Int] -> Bool
    mirroredAt p set =
      let minRangeRight = p + 1
          maxRangeRight = adjust 0
          minRangeLeft = adjust (numRows - 1)
          maxRangeLeft = p
          adjust n | n <= p = p + 1 + (p - n)
                   | n > p = p - (n - p - 1)
          adjusted = map (\n -> (n, p + (p - n) + 1)) set
          filtered = filter (\(n,n') -> n >= minRangeLeft && n <= maxRangeLeft && n' >= minRangeRight && n' <= maxRangeRight) adjusted
       in all (\(_,n') -> n' `elem` set) filtered




findVerticalReflection :: [[Bool]] -> [Int]
findVerticalReflection = findHorizontalReflection . transpose

allPoints :: forall a. [[a]] -> [Top :>> [[a]] :>> [a] :>> a]
allPoints ps = findAll $ zipper ps & fromWithin traverse

findAll :: Top :>> [[a]] :>> [a] -> [Top :>> [[a]] :>> [a] :>> a]
findAll z = findAll1 (z & fromWithin traverse)  ++ maybe [] findAll (rightward z)

findAll1 :: Top :>> [[a]] :>> [a] :>> a -> [Top :>> [[a]] :>> [a] :>> a]
findAll1 z = z:maybe [] findAll1 (rightward z)

part2 :: Parsed -> IO ()
part2 input = do print $ map (\z -> z & view focus) (allPoints [[1,2,3],[4,5,6]])
                 nums <- forM input $ \chart -> do
                   let original = findReflection chart
                   let variants = concat $ findReflections . (\z -> rezip $ z & focus %~ not) <$> allPoints chart
                   let reflection = head . filter (/= original) $ variants
                   print reflection
                   pure $ reflectionScore reflection
                 print . sum $ nums



day13 part args = do let filename = case args of
                                      [] -> "inputs/day13"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
