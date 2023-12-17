{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Day17 (day17) where
import Part (Part (Part1, Part2))
import Data.Map.Strict (Map, fromList)
import Data.Array (listArray, (!), Ix)
import Graph (dijkstraAny)
import qualified Data.Map as Map
import Control.Monad (forM)


newtype Row = Row { unRow :: Int }
  deriving (Show, Ord, Eq, Enum, Ix)
newtype Col = Col { unCol :: Int }
  deriving (Show, Ord, Eq, Enum, Ix)

shiftRow :: Int -> Row -> Row
shiftRow o (Row n) = Row $ n + o
shiftCol :: Int -> Col -> Col
shiftCol o (Col n) = Col $ n + o


type Parsed = (Row,Col,[[((Row,Col),Int)]])
parseInput :: String -> Parsed
parseInput input = let rowsData = lines input
                       rows = length rowsData
                       cols = case rowsData of
                                [] -> 0
                                (r:_) -> length r
                    in (Row rows
                       , Col cols
                       , [ [ ((Row rowNumber, Col colNumber), read [colData])
                           | (colNumber, colData) <- zip [0..] rowData
                           ]
                         | (rowNumber, rowData) <- zip [0..] rowsData
                         ]
                       )

data AllowedDirections = AllowedEastWest | AllowedNorthSouth
  deriving (Show, Ord, Eq)

type GraphParsed = (Map (Row,Col,AllowedDirections) [(Int,(Row,Col,AllowedDirections))],(Row,Col))

mkArray f l = listArray (f 0,f $ length l-1) l

toGrid :: Int -> Int -> Parsed -> GraphParsed
toGrid minDistance maxDistance (height,width,m) =
   let mArr = mkArray Row . fmap (mkArray Col) $ m
    in (fromList [((r,c,allowedDirections),endPoints)
                 |r <- [Row 0..shiftRow (-1) height]
                 ,c <- [Col 0..shiftCol (-1) width]
                 ,allowedDirections <- [AllowedEastWest,AllowedNorthSouth]
                 ,let endPoints = [(cost,(endR,endC,allowed'))
                                  |distance <- [minDistance..maxDistance]
                                  ,turning <- [-1,1]
                                  ,let (endR,endC,allowed',path) =
                                         case allowedDirections of
                                           AllowedEastWest -> (r,shiftCol (distance*turning) c,AllowedNorthSouth,(r,) . (`shiftCol` c) . (turning*) <$> [1..distance])
                                           AllowedNorthSouth -> (shiftRow (distance*turning) r,c,AllowedEastWest,(,c) . (`shiftRow` r) . (turning*) <$> [1..distance])
                                  ,endR >= Row 0 && endR < height
                                  ,endC >= Col 0 && endC < width
                                  ,let cost = sum [snd $ (mArr ! r') ! c'
                                                  |(r',c') <- path
                                                  ]
                                  ]
                 ]
       ,(shiftRow (-1) height, shiftCol (-1) width)
       )

part1 :: Parsed -> IO ()
part1 input
   = do let (graph,(endR,endC)) = toGrid 1 3 input
        let graphWithStart = Map.insert (Row (-10),Col (-10),AllowedEastWest)
                                        [(0,(Row 0,Col 0,AllowedEastWest))
                                        ,(0,(Row 0,Col 0,AllowedNorthSouth))]
                                        graph
         in print $ dijkstraAny graphWithStart (Row (-10),Col (-10),AllowedEastWest) (\(r,c,_) -> r == endR && c == endC)


part2 :: Parsed -> IO ()
part2 input = do let (graph,(endR,endC)) = toGrid 4 10 input
                 let graphWithStart = Map.insert (Row (-10),Col (-10),AllowedEastWest)
                                                 [(0,(Row 0,Col 0,AllowedEastWest))
                                                 ,(0,(Row 0,Col 0,AllowedNorthSouth))]
                                                 graph
                  in print $ dijkstraAny graphWithStart (Row (-10),Col (-10),AllowedEastWest) (\(r,c,_) -> r == endR && c == endC)

day17 part args = do let filename = case args of
                                      [] -> "inputs/day17"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
