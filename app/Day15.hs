module Day15 (day15) where
import Part (Part (Part1, Part2))
import Data.List.Split (splitOn)
import Data.Char (ord, isAlpha)
import Debug.Trace (trace)
import Data.List (partition, tails)
import Data.Array ((!), listArray, Array, (//))
import qualified Data.Array.Base as Array
import Data.Maybe (fromJust)
import Control.Lens (ix, (%~), (&))
import Control.Monad (when)

type Parsed = [String]

parseInput :: String -> Parsed
parseInput = splitOn "," . filter (/= '\n')

hash = fromIntegral . go 0
  where
    go h [] = h
    go h (c:cs) = go (((h + ord c) * 17) `mod` 256) cs

parse2 :: String -> (String, Char, Maybe Integer)
parse2 c = let (label,op:afterLabel) = partition isAlpha c

            in (label,op,if afterLabel == "" then Nothing else Just (read afterLabel))

part1 :: Parsed -> IO ()
part1 i = do print . sum . fmap hash . splitOn "," $ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
             print . sum. fmap hash $  i

runCommand :: (String, Char, Maybe Integer) -> Array Integer [(String, Integer)] -> Array Integer [(String, Integer)]
runCommand (label, op, foclength) boxes =
  let boxNum = hash label
   in boxes & ix boxNum %~ \contents ->
        case op of
          '-' -> [(l, c) | (l, c) <- contents, l /= label]
          '=' -> replaceLabelInBox contents label (fromJust foclength)

replaceLabelInBox ((l,c):contents) label newContents | l == label = (l,newContents):contents
                                                     | otherwise = (l,c):replaceLabelInBox contents label newContents
replaceLabelInBox [] label newContents = [(label, newContents)]

runCommands cs boxes
  = foldl (\ boxes c -> runCommand (parse2 c) boxes) boxes cs

part2 :: Parsed -> IO ()
part2 commands = let boxes = listArray (0,255) [[] | _ <- [0..255]]
                     boxes' = runCommands commands boxes
                  in do print . runCommands ["rn=1"] $ boxes
                        print . runCommands (splitOn "," $ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") $ boxes
                        print boxes'
                        print . score $ boxes'

score :: Array Integer [([Char], Integer)] -> Integer
score = sum . zipWith (\ boxNum contents ->
                         sum . zipWith (curry (\ (slotNum, (_, focLength)) -> boxNum * slotNum * focLength))
                                       [1 .. ]
                                       $ contents
                      ) [1..] . Array.elems

day15 part args = do let filename = case args of
                                      [] -> "inputs/day15"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
