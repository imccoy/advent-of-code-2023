{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day14 (day14) where
import Part (Part (Part1, Part2))
import Data.List (nub, groupBy, sortBy, sort)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map

data Cell = Roll | Fixed | Empty

newtype Row = Row { unRow :: Int }
  deriving (Show, Ord, Eq, Enum)
newtype Col = Col { unCol :: Int }
  deriving (Show, Ord, Eq, Enum)

shiftRow :: Int -> Row -> Row
shiftRow o (Row n) = Row $ n + o
shiftCol :: Int -> Col -> Col
shiftCol o (Col n) = Col $ n + o

type Parsed = ([(Row,Col)],[(Row,Col)])

parseCell 'O' = Roll
parseCell '#' = Fixed
parseCell '.' = Empty

type RawGrid = (Row,Col,[[((Row,Col),Cell)]])
parseInput0 :: String -> RawGrid
parseInput0 input = let rowsData = filter (/= "") $ lines input
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

parseInput :: String -> (Row,Col,Parsed)
parseInput input = let (rows,cols,grid) = parseInput0 input
                    in (rows,
                        cols,
                        mconcat [ case val of
                                    Roll -> ([pos],[])
                                    Fixed -> ([], [pos])
                                    _ -> ([],[])
                                | row <- grid
                                , (pos,val) <- row
                                ])
rollNorth :: Row -> [(Row,Col)] -> [(Row,Col)] -> [(Row,Col)]
rollNorth maxRow fixed = concatMap rollCol . groupBy (\a b -> snd a == snd b) . sortBy (comparing snd)
  where rollCol rollersInCol = let col = snd . head $ rollersInCol
                                   fixedRows = sort [r | (r,c) <- fixed, c == col]
                                   sections = zip (Row 0:map (shiftRow 1) fixedRows)
                                                  (fixedRows ++ [maxRow])
                                   rows = sort $ map fst rollersInCol
                                in [ (shiftRow offset start, col)
                                   | (start,end) <- sections
                                   , let inSection = filter (\r -> r >= start && r < end) rows
                                   , offset <- [0..length inSection - 1]
                                   ]

colToRow (Col c) = Row c
transpose (Row r,Col c) = (Row c, Col r)

rollWest :: Col -> [(Row,Col)] -> [(Row,Col)] -> [(Row,Col)]
rollWest maxCol fixed rollers = transpose <$> rollNorth (colToRow maxCol) (transpose <$> fixed) (transpose <$> rollers)

rollSouth maxRow fixed rollers = vflip maxRow <$> rollNorth maxRow (vflip maxRow <$> fixed) (vflip maxRow <$> rollers)

vflip :: Row -> (Row,Col) -> (Row, Col)
vflip (Row maxRow) (Row r, c) = (Row $ maxRow - 1 - r, c)

rollEast :: Col -> [(Row,Col)] -> [(Row,Col)] -> [(Row,Col)]
rollEast maxCol fixed rollers = transpose <$> rollSouth (colToRow maxCol) (transpose <$> fixed) (transpose <$> rollers)



weighNorth :: Row -> [(Row,Col)] -> Int
weighNorth maxR = sum . map (\(Row r, _) -> unRow maxR - r)

part1 :: (Row,Col,Parsed) -> IO ()
part1 (maxR, maxC, (rollers, fixed)) = do test [1,5,7,8] []
                                          test [1,5,7,8] [0]
                                          test [1,5,7,8] [6]
                                          let grid' = rollNorth maxR fixed rollers
                                          printGrid maxR maxC fixed grid'
                                          print $ weighNorth maxR grid'

printGrid :: Row -> Col -> [(Row, Col)] -> [(Row, Col)] -> IO ()
printGrid maxR maxC fixed rollers = do
  print (length rollers)
  print (maxR, maxC)
  forM_ [(Row 0)..shiftRow (-1) maxR] $ \r -> do
    forM_ [(Col 0)..shiftCol (-1) maxC] $ \c -> do
      if (r,c) `elem` fixed
        then putStr "#"
        else if (r,c) `elem` rollers
               then putStr "O"
               else putStr "."
    putStrLn (" " ++ show (unRow maxR - unRow r))

test :: [Int] -> [Int] -> IO ()
test rollers fixed = do print rollers
                        print fixed
                        print $ rollNorth (Row 100)
                                          (map (\r -> (Row r, Col 0)) fixed)
                                          (map (\r -> (Row r, Col 0)) rollers)

spinCycle maxR maxC fixed rollers = let postNorth = rollNorth maxR fixed rollers
                                        postWest = rollWest maxC fixed postNorth
                                        postSouth = rollSouth maxR fixed postWest
                                        postEast = rollEast maxC fixed postSouth
                                     in [postNorth,postWest,postSouth,postEast]

runIterations 0 maxR maxC fixed rollers = rollers
runIterations n maxR maxC fixed rollers = runIterations (n-1) maxR maxC fixed (last $ spinCycle maxR maxC fixed rollers)

findLoop maxR maxC fixed rollers = go 0 (Map.singleton rollers (-1,-1)) rollers
  where go round seen rollers = let nexts = spinCycle maxR maxC fixed rollers
                                 in go1 round seen 0 rollers nexts
        go1 round seen phase last [] = go (round+1) seen last
        go1 round seen phase last (next:nexts)
          = case Map.insertLookupWithKey (\_ n _ -> n) next (round,phase) seen of
              (Nothing, seen') -> go1 round seen' (phase+1) next nexts
              (Just (cRound,cPhase), seen') -> if cPhase == phase
                                                 then ((round,phase),(cRound,cPhase))
                                                 else go1 round seen' (phase+1) next nexts

part2 :: (Row,Col,Parsed) -> IO ()
part2 (maxR, maxC, (rollers, fixed)) = do let postNorth = rollNorth maxR fixed rollers
                                          printGrid maxR maxC fixed postNorth
                                          let postWest = rollWest maxC fixed postNorth
                                          printGrid maxR maxC fixed postWest
                                          let postSouth = rollSouth maxR fixed postWest
                                          printGrid maxR maxC fixed postSouth
                                          let postEast = rollEast maxC fixed postSouth
                                          printGrid maxR maxC fixed postEast
                                          let loop = findLoop maxR maxC fixed rollers
                                          print loop
                                          let ((loopEnd,0),(loopStart,0)) = loop
                                          let targetIterations = 1000000000
                                          let iterationsAfterPrefix = targetIterations - loopStart
                                          let iterationsRemainingAfterLoop = iterationsAfterPrefix `mod` (loopEnd - loopStart)
                                          let finally = runIterations (loopStart + iterationsRemainingAfterLoop) maxR maxC fixed rollers
                                          print $ weighNorth maxR finally


day14 part args = do let filename = case args of
                                      [] -> "inputs/day14"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
