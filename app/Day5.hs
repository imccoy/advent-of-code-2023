module Day5 (day5) where
import Part (Part (Part1, Part2))

import Control.Monad
import Data.List (sort)
import Debug.Trace
import Text.Parsec

data RangeSpec = RangeSpec { rangeSourceStart :: Integer, rangeDestinationStart :: Integer, rangeLength :: Integer }
  deriving (Show, Eq, Ord)

findDest v [] = v
findDest v (rangeSpec:ranges) | v >= rangeSourceStart rangeSpec && 
                                v < rangeSourceStart rangeSpec + rangeLength rangeSpec
                              = rangeDestinationStart rangeSpec + (v - rangeSourceStart rangeSpec)                              | otherwise
                              = findDest v ranges

transform almanac f seed = let soil = f seed (seedToSoil almanac)
                               fertilizer = f soil (soilToFertilizer almanac)
                               water = f fertilizer (fertilizerToWater almanac)
                               light = f water (waterToLight almanac)
                               temperature = f light (lightToTemperature almanac)
                               humidity = f temperature (temperatureToHumidity almanac)
                               location = f humidity (humidityToLocation almanac)
                            in trace (show [seed, soil, fertilizer, water, light, temperature, humidity, location]) location

data Almanac = Almanac { seedToSoil :: [RangeSpec]
                       , soilToFertilizer :: [RangeSpec]
                       , fertilizerToWater :: [RangeSpec]
                       , waterToLight :: [RangeSpec]
                       , lightToTemperature :: [RangeSpec]
                       , temperatureToHumidity :: [RangeSpec]
                       , humidityToLocation :: [RangeSpec]
                       }
  deriving (Show)

type Parsed = ([Integer], Almanac)

parseSection :: (Monad f) => String -> ParsecT String u f [RangeSpec]
parseSection header = do void $ string header
                         void $ string " map:"
                         void newline
                         range <- endBy1 (do targetStart <- read <$> many1 digit
                                             void space
                                             sourceStart <- read <$> many1 digit
                                             void space
                                             length <- read <$> many1 (try digit)
                                             pure $ RangeSpec { rangeSourceStart = sourceStart
                                                              , rangeDestinationStart = targetStart
                                                              , rangeLength = length
                                                              } 
                                         )
                                         newline
                         void newline <|> eof
                         pure range

parseAlmanac = do void $ string "seeds: "
                  seedNumbers <- fmap read <$> sepBy (many1 digit) (char ' ')
                  void $ newline
                  void $ newline
                  almanac <- Almanac <$> (sort <$> parseSection "seed-to-soil")
                                     <*> (sort <$> parseSection "soil-to-fertilizer")
                                     <*> (sort <$> parseSection "fertilizer-to-water")
                                     <*> (sort <$> parseSection "water-to-light")
                                     <*> (sort <$> parseSection "light-to-temperature")
                                     <*> (sort <$> parseSection "temperature-to-humidity")
                                     <*> (sort <$> parseSection "humidity-to-location")
                  pure (seedNumbers, almanac)

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseAlmanac () "none" 

part1 :: Parsed -> IO ()
part1 (seeds, almanac) = putStrLn . show . sort $ (transform almanac findDest <$> seeds)

pairSeeds [] = []
pairSeeds (min:len:seeds) = (min,len):(pairSeeds seeds)

findDestRanges vranges rangeSpecs = concat $ map (`go` rangeSpecs) vranges
  where go (min,len) [] = [(min,len)]
        go (min,len) (rs:rss) =
          let rsSource = rangeSourceStart rs
              rsDest = rangeDestinationStart rs
              rsLen = rangeLength rs
              next | min < rsSource && min + len >= rsSource   = (min,rsSource-min):(go (rsSource,len-(rsSource-min)) (rs:rss))
                   | min >= rsSource && min < rsSource + rsLen = let offset = min - rsSource
                                                                     lenHere = rsLen - offset
                                                                  in if min + len < rsSource + rsLen 
                                                                       then [(rsDest + (min - rsSource), len)]
                                                                       else (rsDest + (min - rsSource), lenHere):(go (min + lenHere, len - lenHere) rss)
                   | otherwise                                 = go (min,len) rss 
           in next

demo rangeSpec range expected = putStrLn $ show rangeSpec ++ show range ++ show (findDestRanges [range] [rangeSpec]) ++ show expected

part2 :: Parsed -> IO ()
part2 (seeds0, almanac) = do demo (RangeSpec { rangeSourceStart = 10, rangeDestinationStart = 15, rangeLength = 5 }) (0,5) [(0,5)]
                             demo (RangeSpec { rangeSourceStart = 10, rangeDestinationStart = 15, rangeLength = 5 }) (10,2) [(15,2)]
                             demo (RangeSpec { rangeSourceStart = 10, rangeDestinationStart = 15, rangeLength = 5 }) (13,2) [(18,2)]
                             demo (RangeSpec { rangeSourceStart = 10, rangeDestinationStart = 15, rangeLength = 5 }) (13,3) [(18,2),(15,1)]
                             demo (RangeSpec { rangeSourceStart = 10, rangeDestinationStart = 15, rangeLength = 5 }) (10,8) [(15,5),(15,3)]
                             demo (RangeSpec { rangeSourceStart = 10, rangeDestinationStart = 15, rangeLength = 5 }) (16,8) [(16,8)]
                             putStrLn . show . sort $ (transform almanac findDestRanges $ pairSeeds seeds0)

day5 part args = do let filename = case args of
                                     [] -> "inputs/day5"
                                     [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
