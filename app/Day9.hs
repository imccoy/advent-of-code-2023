module Day9 (day9) where
import Part (Part (Part1, Part2))
import Debug.Trace (trace)

type Parsed = [[Integer]]

parseInput :: String -> Parsed
parseInput = map (map read . words) . lines

next ns = let diffs = zipWith (-) (tail ns) ns
              nextDiff = next diffs
           in if all (==0) diffs
                then last ns
                else last ns + nextDiff

part1 :: Parsed -> IO ()
part1 input = do print (next [1,2,3,4])
                 print (next [1,3,6,10,15,21])
                 print (next [10,13,16,21,30,45])
                 print . sum $ next <$> input
                 print . sum $ extend (\ns n -> last ns + n) <$> input

prev ns = let diffs = zipWith (-) (tail ns) ns
              prevDiff = prev diffs
           in if all (==0) diffs
                then head ns
                else head ns - prevDiff

extend f ns = let diffs = zipWith (-) (tail ns) ns
                  diff' = extend f diffs
               in f ns (if all (==0) diffs then 0 else diff')


part2 :: Parsed -> IO ()
part2 input = do print . sum $ prev <$> input
                 print . sum $ extend (\ns n -> head ns - n) <$> input

day9 part args = do let filename = case args of
                                     [] -> "inputs/day9"
                                     [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
