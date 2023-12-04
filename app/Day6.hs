module Day6 (day6) where
import Part (Part (Part1, Part2))

import Control.Monad
import Text.Parsec

type Parsed = [(Int,Int)]

parseRaces = do times <- do void $ string "Time:"
                            spaces
                            manyTill (do n <- read <$> many1 digit
                                         many (char ' ')
                                         pure n
                                     ) newline
                distances <- do void $ string "Distance:"
                                spaces
                                manyTill (do n <- read <$> many1 digit
                                             many (char ' ')
                                             pure n
                                         ) (eof <|> void newline)
                pure $ zip times distances
 

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseRaces () "none" 

distancesForRace raceLength = do holdTime <- [1..raceLength-1]
                                 let moveTime = raceLength - holdTime
                                 pure $ holdTime * moveTime

betterDistances distances recordDistance = filter (> recordDistance) distances

part1 :: Parsed -> IO ()
part1 rounds = putStrLn . show . product $ [length $ betterDistances (distancesForRace time) distance | (time,distance) <- rounds]

concatNumbers = go 0
  where go n [] = n
        go n (a:as) = go (makeRoom n a + a) as
        makeRoom n 0 = n
        makeRoom n a = makeRoom (n * 10) (a `div` 10)
fixKerning rounds = (concatNumbers $ fst <$> rounds, concatNumbers $ snd <$> rounds)

part2 :: Parsed -> IO ()
part2 rounds = let (time,distance) = fixKerning rounds
                in do putStrLn . show $ concatNumbers [1,2,3]
                      putStrLn . show $ concatNumbers [123,45,6,78]
                      putStrLn . show . length $ betterDistances (distancesForRace time) distance

day6 part args = do let filename = case args of
                                     [] -> "inputs/day6"
                                     [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
