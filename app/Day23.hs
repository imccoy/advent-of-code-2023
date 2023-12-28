{-# LANGUAGE TupleSections #-}
module Day23 (day23) where
import Part (Part (Part1, Part2))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.List ((\\), sort)
import Debug.Trace (trace)
import qualified Data.Set as Set

data Direction = North | South | East | West
  deriving (Show)

data Cell = Empty | Wall | Slope Direction
  deriving (Show)


newtype Row = Row { unRow :: Int }
  deriving (Show, Ord, Eq)
newtype Col = Col { unCol :: Int }
  deriving (Show, Ord, Eq)

shiftRow :: Int -> Row -> Row
shiftRow o (Row n) = Row $ n + o
shiftCol :: Int -> Col -> Col
shiftCol o (Col n) = Col $ n + o

type Parsed = (Row,Col,[[((Row,Col),Cell)]])

parseCell '.' = Empty
parseCell '#' = Wall
parseCell '^' = Slope North
parseCell '>' = Slope East
parseCell 'v' = Slope South
parseCell '<' = Slope West

type RawGrid = (Row,Col,[[((Row,Col),Cell)]])
parseInput :: String -> RawGrid
parseInput input = let rowsData = lines input
                       rows = length rowsData
                       cols = case rowsData of
                                [] -> 0
                                (r:_) -> length r
                    in (Row rows
                       , Col cols
                       , [ [ ((Row rowNumber, Col colNumber), parseCell colData)
                           | (colNumber, colData) <- zip [0..] rowData
                           ]
                         | (rowNumber, rowData) <- zip [0..] rowsData
                         ]
                       )

toGraph (height,width,layout) = let cells = Map.fromList . concat $ layout
                                    adjs (row,col) c = case c of
                                                         Wall -> []
                                                         Slope dir -> [neighbour (row,col) dir]
                                                         Empty -> mapMaybe (emptyNeighbour (row,col)) [North,South,East,West]
                                    neighbour (row,col) North = (shiftRow (-1) row,col)
                                    neighbour (row,col) East = (row,shiftCol 1 col)
                                    neighbour (row,col) South = (shiftRow 1 row,col)
                                    neighbour (row,col) West = (row,shiftCol (-1) col)
                                    emptyNeighbour (row,col) dir = let n = neighbour (row,col) dir
                                                                    in case Map.lookup n cells of
                                                                         Just Wall -> Nothing
                                                                         Just _ -> Just n
                                                                         Nothing -> Nothing
                                 in Map.fromList $ [ ((row,col),adjs (row,col) c)
                                                   | ((row,col),c) <- concat layout
                                                   ]
simplifyGraph :: Map.Map (Row,Col) [(Row,Col)] -> Map.Map (Row,Col) [((Row,Col),Int)]
simplifyGraph graph = go Map.empty $ Map.keys intersections
  where intersections = Map.filter (\e -> length e /= 2) graph
        go nodes [] = nodes
        go nodes (p:ps) = go (go1 nodes p) ps
        go1 nodes p = Map.insert p (pathEnds p) nodes
        pathEnds p = pathEnd 1 p <$> fromMaybe [] (Map.lookup p graph)
        pathEnd l prev next = case fromMaybe [] (Map.lookup next graph) \\ [prev] of
                                [one] -> pathEnd (l+1) next one
                                _ -> (next,l)

part1 :: Parsed -> IO ()
part1 (height,width,layout) = let graph = toGraph (height,width,layout)
                                  start = (Row 0, Col 1)
                                  end = (shiftRow (-1) height, shiftCol (-2) width)
                               in do print $ maximum $ findLongestPath graph start end
                                     print $ sort $ findLongestPath graph start end
                                     print $ maximum $ findLongestPath' (Map.map (fmap (,1)) graph) start end
                                     print $ sort$ findLongestPath' (Map.map (fmap (,1)) graph) start end
                                     print $ maximum $ findLongestPath' (simplifyGraph graph) start end
                                     print $ sort $ findLongestPath' (simplifyGraph graph) start end

findLongestPath :: Map.Map (Row,Col) [(Row,Col)] -> (Row, Col) -> (Row, Col) -> [Int]
findLongestPath graph start end = fmap (length . fst) $ go [(Set.empty, start)]
  where go [] = []
        go ps = let (dones, nexts) = unzip $ go1 <$> ps
                 in concat dones ++ go (concat nexts)
        go1 (path,place) | place == end = ([(path,place)], [])
                         | otherwise    = let nexts = filter (`Set.notMember` path) $ fromMaybe [] (Map.lookup place graph)
                                          in ([], (Set.insert place path,) <$> nexts)


findLongestPath' :: Map.Map (Row,Col) [((Row,Col),Int)] -> (Row, Col) -> (Row, Col) -> [Int]
findLongestPath' graph start end = go [(Set.empty, 0, start)]
  where go [] = []
        go ps = let (dones, nexts) = unzip $ go1 <$> ps
                 in concat dones ++ go (concat nexts)
        go1 (path,dist0,place) | place == end = ([dist0], [])
                               | otherwise    = ([],[(Set.insert place path,dist0 + dist,next)
                                                    | (next,dist) <- fromMaybe [] (Map.lookup place graph)
                                                    , Set.notMember next path
                                                    
                                                    ])



changeSlopes = map (map (\((row,col),c) -> case c of
                                             Slope _ -> ((row,col),Empty)
                                             _ -> ((row,col),c)
                        )
                   )


part2 :: Parsed -> IO ()
part2 (height,width,layout) = let graph = toGraph (height,width,changeSlopes layout)
                                  start = (Row 0, Col 1)
                                  end = (shiftRow (-1) height, shiftCol (-2) width)
                               in print $ maximum $ findLongestPath' (simplifyGraph graph) start end

day23 part args = do let filename = case args of
                                      [] -> "inputs/day23"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
