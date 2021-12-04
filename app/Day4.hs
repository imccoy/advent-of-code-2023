module Day4 (day4) where
import Part (Part (Part1, Part2))

import Data.List (partition)
import Data.List.Split (splitWhen, wordsBy)
import Data.Maybe (fromMaybe)
import Debug.Trace

data Board = Board [[Maybe Int]]
  deriving (Show)

parseBingo :: [String] -> ([Int], [Board])
parseBingo (numbers:_:rest) = (map read . splitWhen (== ',') $ numbers
                              ,map parseBoard . splitWhen (== "") $ rest)
  where parseBoard :: [String] -> Board
        parseBoard = Board . map (map (Just . read) . wordsBy (== ' '))

removeNumber :: Int ->[Board] -> [Board]
removeNumber n = map (\(Board b) -> Board $ map (map (\x -> if x == Just n then Nothing else x)) b)

isWinningBoard :: Board -> Bool
isWinningBoard (Board cells) = rowWin cells || rowWin (transpose cells)  || diagonalWin cells || diagonalWin (transpose cells)

rowWin cells = any (all (== Nothing)) cells

diagonalWin [] = True
diagonalWin ((Nothing:_):rest) = diagonalWin $ map (drop 1) rest
diagonalWin (((Just _):_):_) = False

transpose (r:rows) = prependToRows r (transpose rows)
  where
    prependToRows (c:cs) (col:cols) = (c:col):(prependToRows cs cols)
    prependToRows r [] = map pure r
transpose [] = []

input :: IO ([Int], [Board])
input = parseBingo . lines <$> readFile "inputs/day4"


findWinners [] _ = []
findWinners (num:numbers) boards = let boards' = removeNumber num boards
                                       (winners, stillInPlay) = partition isWinningBoard boards'
                                    
                                    in map (\w -> (num,w)) winners ++ findWinners numbers stillInPlay

part1 = do
  (numbers, boards) <- input
  let (lastNumberCalled, (Board winningBoard)) = head $ findWinners numbers boards
  putStrLn . show $ lastNumberCalled * (sum . map (fromMaybe 0) . concat $ winningBoard)

part2 = do
  (numbers, boards) <- input
  let (lastNumberCalled, (Board winningBoard)) = head . reverse $ findWinners numbers boards
  putStrLn . show $ lastNumberCalled * (sum . map (fromMaybe 0) . concat $ winningBoard)

day4 Part1 _ = part1
day4 Part2 _ = part2
