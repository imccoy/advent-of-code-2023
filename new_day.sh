day=$1


sed "s/NEW/$1/g" > app/Day${day}.hs << EOF
module DayNEW (dayNEW) where
import Part (Part (Part1, Part2))

type Parsed = [String]

parseInput :: String -> Parsed
parseInput = lines

part1 :: Parsed -> IO ()
part1 _ = putStrLn "part1"

part2 :: Parsed -> IO ()
part2 _ = putStrLn "part2"

dayNEW part args = do let filename = case args of
                                      [] -> "inputs/dayNEW"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
EOF

sed -i "" "s/-- NEXT DAY IMPORT/import Day$day (day$day)\n-- NEXT DAY IMPORT/;s/-- NEXT DAY CASE/\"$day\" -> Just day$day\n    -- NEXT DAY CASE/" app/main.hs

curl -H "Cookie: session=$(cat .session-cookie)" https://adventofcode.com/2023/day/$day/input > inputs/day$day
