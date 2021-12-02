module Day1 (day1) where
import Part (Part (Part1, Part2))
import Data.Foldable (foldl')

input :: IO String
input = readFile "inputs/day1"

inputNumbers :: IO [Int]
inputNumbers = fmap read . lines <$> input

countIncreases :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
countIncreases (Nothing, count) current = (Just current, count)
countIncreases (Just last, count) current
  | current > last = let count' = count + 1
                      in seq count' (Just current, count')
  | otherwise      = (Just current, count)

countIncreasesSlicker numbers = length . filter (\(x,y) -> x > y) $ zip (drop 1 numbers) numbers

part1 = do
  putStrLn =<< show . foldl' countIncreases (Nothing, 0) <$> inputNumbers
  putStrLn =<< show . countIncreasesSlicker <$> inputNumbers

windows numbers = zip3 (drop 2 numbers) (drop 1 numbers) numbers
windowSums = map (\(a,b,c) -> a + b + c)

part2 = do
  putStrLn =<< show . countIncreasesSlicker . windowSums . windows <$> inputNumbers

day1 Part1 _ = part1
day1 Part2 _ = part2
