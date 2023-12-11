{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM" #-}
module Day11 (day11) where
import Part (Part (Part1, Part2))
import Data.List (uncons)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Control.Monad (forM_)

type Parsed = [[Bool]]

parseInput :: String -> Parsed
parseInput i = [[char == '#' | char <- line] | line <- lines i]

expandRows [] = []
expandRows (row:rows) | all not row = row:row:expandRows rows
                      | otherwise          = row:expandRows rows

expandCols chart = case unzip <$> sequence (uncons <$> chart) of
                     Just (heads,tails) ->
                       if all not heads
                         then zipWith (\h t -> h:h:t) heads (expandCols tails)
                         else zipWith (:) heads (expandCols tails)
                     Nothing -> repeat []

extractCoords = go 0
  where go _ [] = []
        go rowNum (row:rows) = goR rowNum 0 row ++ go (rowNum + 1) rows
        goR _ _ [] = []
        goR rowNum colNum (True:cells) = (rowNum,colNum):goR rowNum (colNum+1) cells
        goR rowNum colNum (False:cells) = goR rowNum (colNum+1) cells

cross [] r = r
cross (x:xs) r = cross xs (cross1 xs r)
  where cross1 [] r = r
        cross1 (x':xs') r = cross1 xs' ((x,x'):r)

sumDistances pairs = sum [abs (r2 - r1) + abs (c2 - c1) | ((r1,c1),(r2,c2)) <- pairs]

part1 :: Parsed -> IO ()
part1 chart = let expanded = expandCols . expandRows $ chart
                  coords = extractCoords expanded
                  pairs = cross coords []
               in print $ sumDistances pairs

findEmptyRows :: [[Bool]] -> [Integer]
findEmptyRows = go 0
  where go n (row:rows) | all not row = n:go (n+1) rows
                        | otherwise = go (n+1) rows
        go n [] = []


findEmptyCols = go 0
  where go n [] = []
        go n chart = case unzip <$> sequence (uncons <$> chart) of
                       Just (heads,tails) ->
                         if all not heads
                           then n:go (n+1) tails
                           else go (n+1) tails
                       Nothing -> []

printUniverse coords = let width = maximum $ snd <$> coords
                           height = maximum $ fst <$> coords
                        in forM_ [0..height] $ \r ->
                             do forM_ [0..width] $ \c ->
                                  if (r,c) `elem` coords
                                    then putStr "#"
                                    else putStr "."
                                putStr "\n"

part2 :: Parsed -> IO ()
part2 chart = let coords = extractCoords chart
                  emptyRows = findEmptyRows chart
                  emptyCols = findEmptyCols chart
               in do print emptyRows
                     print emptyCols
                     printUniverse coords
                     printUniverse (expandUniverse coords emptyRows emptyCols 2)
                     print (sumDistances (cross (expandUniverse coords emptyRows emptyCols 2) []))
                     print (sumDistances (cross (expandUniverse coords emptyRows emptyCols 10) []))
                     print (sumDistances (cross (expandUniverse coords emptyRows emptyCols 100) []))
                     print (sumDistances (cross (expandUniverse coords emptyRows emptyCols 1000000) []))

expandUniverse :: [(Integer, Integer)] -> [Integer] -> [Integer] -> Integer -> [(Integer,Integer)]
expandUniverse coords emptyRows emptyCols shiftBy
  = map (\(r,c) -> 
           let rSkip = fromIntegral . length . filter (<r) $ emptyRows
               cSkip = fromIntegral . length . filter (<c) $ emptyCols
            in (r + rSkip * shiftBy - rSkip, c + cSkip * shiftBy - cSkip)
        )
        coords


day11 part args = do let filename = case args of
                                      [] -> "inputs/day11"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
