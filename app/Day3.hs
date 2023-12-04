module Day3 (day3) where
import Part (Part (Part1, Part2))

import Control.Monad
import Data.Char
import qualified Data.Set as Set

data Cell = Empty | Digit Int | Symbol Char
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
parseCell c | isDigit c = Digit (read [c])
            | otherwise = Symbol c

type RawGrid = (Row,Col,[[((Row,Col),Cell)]])
parseInput0 :: String -> RawGrid
parseInput0 input = let rowsData = lines input
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

mergeNs grid = [mergeRowNs row | row <- grid]
  where mergeRowNs [] = []
        mergeRowNs (h:t) = case (h, mergeRowNs t) of
                             ((loc, Digit n), ((locs, Digit m):rest)) -> (loc:locs, (Digit $ n * (10 ^ length locs) + m)):rest
                             ((loc,v), rest) -> ([loc],v):rest

part1 :: Parsed -> IO ()
part1 (_,_,locs) = let activeLocations = Set.fromList [shiftOffset offset coord
                                                      |row <- locs
                                                      ,(coord,Symbol _) <- row
                                                      ,offset <- offsets
                                                      ]
                       partNumbers = [ d
                                     | (coords, Digit d) <- concat $ mergeNs locs
                                     , any (`Set.member` activeLocations) coords
                                     ]
                    in do forM_ (mergeNs locs) $ \loc -> do
                            putStrLn $ show $ map snd loc
                          forM_ (mergeNs locs) $ \loc -> do
                            putStrLn $ show loc
                          putStrLn . show . sum $ partNumbers
                                                  

shiftOffset (rOffset, cOffset) (rowNum, colNum) = (shiftRow rOffset rowNum, shiftCol cOffset colNum)
offsets = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]

part2 :: Parsed -> IO ()
part2 (_,_,locs) = let numbers = [ (coords, d) | (coords, Digit d) <- concat $ mergeNs locs ]
                       potentialGearCoords = [ coords | (coords, Symbol '*') <- concat locs]
                       gearRatios = [ product gearNumbers
                                    | coord <- potentialGearCoords
                                    , let gearNumbers = [d | (coords, d) <- numbers, any (\offset -> shiftOffset offset coord `elem` coords) offsets]
                                    , length gearNumbers == 2
                                    ]
                    in putStrLn $ show $ sum gearRatios

day3 part args = do let filename = case args of
                                      [] -> "inputs/day3"
                                      [f] -> f
                    inputs <- parseInput0 <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
