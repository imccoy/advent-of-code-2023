{-# LANGUAGE BangPatterns #-}
module Day12 (day12) where
import Part (Part (Part1, Part2))

import Text.Parsec
import Control.Monad (forM)
import Data.List (intercalate, tails, nub)
import Debug.Trace (trace)
import Data.Map (Map)
import qualified Data.Map as Map

data Presence = Working | Broken | Unknown
  deriving (Show)

type Parsed = [([Presence], [Int])]

parseRules = do presences <- manyTill ((char '#' >> pure Working)
                                        <|> (char '.' >> pure Broken)
                                        <|> (char '?' >> pure Unknown)
                                      ) (char ' ')
                counts <- sepBy (read <$> many digit) (char ',')
                pure (presences, counts)


parseInput :: String -> Parsed
parseInput = fmap parseLine . filter (not . null) . lines
parseLine = either (error . show) id . runParser parseRules () "none"

countWays :: [Presence] -> [Int] -> Int
countWays presences counts
  = takeHere presences counts + skipHere presences counts
  where takeHere [] [0] = 1
        takeHere [] [] = 1
        takeHere [] _ = 0
        takeHere _ [] = 0
        takeHere (Broken:rest) (0:ns) = countWays rest ns
        takeHere (Unknown:rest) (0:ns) = countWays rest ns
        takeHere (Broken:_) _ = 0
        takeHere (_:rest) (n:ns) = takeHere rest ((n-1):ns)
        skipHere (Working:_) _ = 0
        skipHere (_:rest) ns = countWays rest ns
        skipHere [] _ = 0

countWaysA :: [Presence] -> [Int] -> Int -> Int
countWaysA presences counts !acc
  = takeHere presences counts (skipHere presences counts acc)
  where takeHere [] [0] !acc = acc + 1
        takeHere [] [] !acc = acc + 1
        takeHere [] _ !acc = acc
        takeHere _ [] !acc = acc
        takeHere (Broken:rest) (0:ns) !acc = countWaysA rest ns acc
        takeHere (Unknown:rest) (0:ns) !acc = countWaysA rest ns acc
        takeHere (Broken:_) _ !acc = acc
        takeHere (_:rest) (n:ns) !acc = takeHere rest ((n-1):ns) acc
        skipHere (Working:_) _ !acc = acc
        skipHere (_:rest) ns !acc = countWaysA rest ns acc
        skipHere [] _ !acc = acc


test presences counts expected = do
  putStrLn $ "TEST " ++ show presences ++ " " ++ show counts ++ " " ++ show expected ++ " -> " ++ show (countWays presences counts)

part1 :: Parsed -> IO ()
part1 input = do
  test [Working,Working,Working] [3] 1
  test [Working,Working,Working,Broken,Working] [3,1] 1
  test [Unknown,Unknown] [1] 2
  test [Broken,Unknown,Unknown] [1] 2
  test [Unknown,Unknown,Broken] [1] 2
  test [Unknown] [1] 1
  test [Unknown,Unknown,Unknown] [2] 2
  test [Unknown,Working,Unknown] [2] 2
  testVl [Working] [1]
  testVl [Working,Working,Broken,Working] [2,1]
  testVl [Working,Unknown,Broken,Working] [2,1]
  testVl [Working,Unknown,Unknown,Working] [2,1]
  testVl [Unknown,Working,Unknown,Unknown,Working,Unknown] [2,2]
  counts <- forM input $ \(presences, counts) ->
    do let count = countWays presences counts
       putStrLn $ show count ++ " " ++ show presences ++ show counts
       putStrLn $ show (countWays' presences counts)
       pure count
  print $ sum counts

testVl :: [Presence] -> [Int] -> IO ()
testVl presences counts = do
   putStrLn $ show presences ++ " " ++ show (countWays presences counts)
   print (countWays' presences counts)

couldBeBroken Working = False
couldBeBroken _ = True

countWays' presences counts = sum . Map.elems . Map.mapMaybeWithKey (\k v -> if k >= ending then Just v else Nothing) $ go (Map.singleton 0 1) counts
  where
    go sws (c:cs) = let sws' = go1 (Map.toList sws) c
                     in go (Map.fromListWith (+) sws') cs
    go sws [] = sws
    go1 ((s,w):sws) c
      = [ (c+s+skip+1,w)
        | skip <- 0:canSkip 1 (drop s presences)
        , canTake c (drop (s+skip) presences)
        ] ++ go1 sws c
    go1 [] c = []
    canTake 0 (Working:_) = False
    canTake 0 _ = True
    canTake n (Broken:ps) = False
    canTake n (_:ps) = canTake (n-1) ps
    canTake n [] = False
    canSkip _ [] = []
    canSkip n (Working:_) = []
    canSkip n (_:ps) = n:canSkip (n+1) ps
    ending = findEnding (length presences) (reverse presences)
    findEnding n [] = n
    findEnding n (Working:_) = n
    findEnding n (_:ps) = findEnding (n-1) ps

part2 :: Parsed -> IO ()
part2 input = do
  counts <- forM input $ \(presences, counts) ->
    do let unfoldedPresences = intercalate [Unknown] (replicate 5 presences)
       let unfoldedCounts = concat $ replicate 5 counts
       let count = countWays' unfoldedPresences unfoldedCounts
       print count
       pure count
  print $ sum counts



day12 part args = do let filename = case args of
                                      [] -> "inputs/day12"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
