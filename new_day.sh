day=$1


sed "s/NEW/$1/g" > app/Day${day}.hs << EOF
module DayNEW (dayNEW) where
import Part (Part (Part1, Part2))

input :: IO String
input = readFile "inputs/dayNEW"


part1 = putStrLn "part1"
part2 = putStrLn "part2"

dayNEW Part1 _ = part1
dayNEW Part2 _ = part2
EOF

sed -i "" "s/-- NEXT DAY IMPORT/import Day$day (day$day)\n-- NEXT DAY IMPORT/;s/-- NEXT DAY CASE/\"$day\" -> Just day$day\n    -- NEXT DAY CASE/" app/main.hs

curl -H "Cookie: session=$(cat .session-cookie)" https://adventofcode.com/2021/day/$day/input > inputs/day$day
