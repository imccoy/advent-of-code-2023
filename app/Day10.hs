module Day10 (day10) where
import Part (Part (Part1, Part2))
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.List as List
import Debug.Trace (trace)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (forM_)
import Data.Char (chr)

type Parsed = Map (Int,Int) [(Int,Int)]

applyOffset (row, col) (dRow, dCol) = (row + dRow, col + dCol)

parseMap = Map.unions . concat <$> manyTill  (manyTill (do sourcePos <- getPosition
                                                           let row = sourceLine sourcePos
                                                           let pos = (row, sourceColumn sourcePos)

                                                           choice [char 'S' >> pure (Map.fromList [((-100, -100), [pos]),
                                                                                                   (pos, [ applyOffset pos (-1,0)
                                                                                                         , applyOffset pos (1,0)
                                                                                                         , applyOffset pos (0,-1)
                                                                                                         , applyOffset pos (0,1)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char '|' >> pure (Map.fromList [(pos, [ applyOffset pos (-1,0)
                                                                                                         , applyOffset pos (1,0)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char '-' >> pure (Map.fromList [(pos, [ applyOffset pos (0,-1)
                                                                                                         , applyOffset pos (0,1)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char 'L' >> pure (Map.fromList [(pos, [ applyOffset pos (-1,0)
                                                                                                         , applyOffset pos (0,1)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char 'J' >> pure (Map.fromList [(pos, [ applyOffset pos (-1,0)
                                                                                                         , applyOffset pos (0,-1)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char '7' >> pure (Map.fromList [(pos, [ applyOffset pos (1,0)
                                                                                                         , applyOffset pos (0,-1)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char 'F' >> pure (Map.fromList [(pos, [ applyOffset pos (1,0)
                                                                                                         , applyOffset pos (0,1)
                                                                                                         ]
                                                                                                   )])
                                                                  ,char '.' >> pure Map.empty
                                                                  ]
                                                        ) newline) eof

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseMap () "none"

part1 :: Parsed -> IO ()
part1 chart = let loop = findLoopFromChart chart
               in print (length loop `div` 2)

findLoopFromChart chart = let [start] = fromJust $ Map.lookup (-100, -100) chart
                              frontier = [(node,[start]) | node <- fromJust $ Map.lookup start chart]
                           in findLoop chart start frontier

findLoop :: Parsed -> (Int,Int) -> [((Int, Int), [(Int,Int)])] -> [(Int,Int)]
findLoop parsed start ((current, path@(prev:_)):frontier)
  | start == current && not (List.null path) = path
  | otherwise = findLoop parsed start $ [ (next, current:path)
                                        | next <- fromMaybe [] $ Map.lookup current parsed
                                        , next /= prev
                                        ] ++ frontier

fullyConnected1 :: (Int,Int) -> [(Int,Int)]
fullyConnected1 (r,c) = [ (row,col)
                        | offset <- [(-1,0),(1,0),(0,-1),(0,1)]
                        , let (row,col) = applyOffset (r,c) offset
                        , row >= 0 && row <= 140 * 3 && col >= 0 && col <= 140 * 3
                        ]

bannedCells path@(start:_) = concat $ zipWith bannedCellPair path (tail path ++ [start])

bannedCellPair p1@(r1,c1) p2@(r2, c2) | r1 == r2 && c1 < c2 = [applyOffset p1 (1,1),applyOffset p1 (1,2),applyOffset p2 (1,0),applyOffset p2 (1,1)]
                                      | r1 == r2 && c2 < c1 = reverse $ bannedCellPair p2 p1
                                      | c1 == c2 && r1 < r2 = [applyOffset p1 (1,1),applyOffset p1 (2,1),applyOffset p2 (0,1),applyOffset p2 (1,1)]
                                      | c1 == c2 && r2 < r1 = reverse $ bannedCellPair p2 p1
                                      | otherwise = error $ "Can't prune cells: " ++ show p1 ++ " " ++ show p2

transitiveClosure :: Set (Int,Int) -> (Int,Int) -> Set (Int,Int)
transitiveClosure banned start = go [start] Set.empty
  where go [] visited = visited
        go (here:rest) visited
         | Set.member here visited = go rest visited
         | otherwise               = let neighbours = filter (\n -> not (Set.member n banned)) $ fullyConnected1 here
                                      in go (neighbours ++ rest) (Set.insert here visited)


part2 :: Parsed -> IO ()
part2 chart = let loop@(start@(rStart,cStart):_) = findLoopFromChart chart
                  prunePoints = Set.fromList $ bannedCells [(r * 3, c * 3) | (r,c) <- loop]
               in do
                     print loop
                     print [(r * 3, c * 3) | (r,c) <- loop]
                     print $ bannedCells [(r * 3, c * 3) | (r,c) <- loop]
                     forM_ [(0,0),(0,2),(2,2),(2,0)] $ \cornerOffset ->
                       do let corner = applyOffset (rStart * 3, cStart * 3) cornerOffset
                          let withinPruned = transitiveClosure prunePoints corner
                          let midsWithinPruned = Set.filter (\(r,c) -> r `mod` 3 == 1 && c `mod` 3 == 1) withinPruned
                          forM_ [0..141] $ \row ->
                            do forM_ [0..141] $ \col ->
                                 if Set.member (row*3+1,col*3+1) midsWithinPruned
                                   then putStr [chr 0x25A0] 
                                   else if (row,col) `elem` loop
                                          then putStr "+"
                                          else putStr "."
                               putStr "\n"
                          print (Set.size midsWithinPruned)


day10 part args = do let filename = case args of
                                      [] -> "inputs/day10"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
