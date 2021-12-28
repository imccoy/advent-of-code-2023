module Day25 (day25) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Debug.Trace (trace)

import Part (Part (Part1, Part2))

fieldWidth = 138
fieldHeight = 136

{-
fieldWidth = 9
fieldHeight = 8
-}

data Facing = South | East
  deriving (Show, Eq)

facingFromChar '>' = Just East
facingFromChar 'v' = Just South
facingFromChar '.' = Nothing

facingToChar (Just East) = '>'
facingToChar (Just South) = 'v'
facingToChar (Nothing) = '.'


nextPosition (x, y) South = (x, if y + 1 <= fieldHeight then y + 1 else 0)
nextPosition (x, y) East  = (if x + 1 <= fieldWidth then x + 1 else 0, y)

cukeClear position facing cukes = isNothing $ Map.lookup (nextPosition position facing) cukes

canMove cukes candidates = filter (\(position, facing) -> cukeClear position facing cukes) $ candidates

herd :: Facing -> Map (Int, Int) Facing -> [((Int, Int), Facing)]
herd herdFacing = filter ((== herdFacing) . snd) . Map.assocs

moveAll :: [((Int,Int),Facing)] -> Map (Int,Int) Facing -> Map (Int,Int) Facing
moveAll movers cukes = foldr (\(position, facing) cukes -> Map.insert (nextPosition position facing) facing cukes)
                             (foldr Map.delete cukes (fst <$> movers))
                             movers

showCukes :: Map (Int, Int) Facing -> String
showCukes cukes = concat $ [ (fmap (\x -> facingToChar $ Map.lookup (x,y) cukes) [0..fieldWidth]) ++ "\n"
                           | y <- [0..fieldHeight]]

step cukes = let eastMovers = canMove cukes $ herd East cukes
                 eastMoved = moveAll eastMovers cukes
                 southMovers = canMove eastMoved $ herd South eastMoved
                 southMoved = moveAll southMovers eastMoved
              in (southMoved, length eastMovers + length southMovers)

runToStop stepNumber cukes = let (cukes', moved) = trace ("STEP " ++ show stepNumber ++ "\n" ++ showCukes cukes ++ "\n\n") step cukes
                              in case moved of
                                   0 -> stepNumber
                                   _ -> runToStop (stepNumber + 1) cukes'

parseInput :: String -> Map (Int,Int) Facing
parseInput = Map.mapMaybe (\v -> v) . Map.fromList . concat . fmap (\(y, xs) -> fmap (\(x, o) -> ((x, y), facingFromChar o)) xs) . zip [0..] . fmap (zip [0..]) . lines

part1 :: Map (Int,Int) Facing -> IO ()
part1 cukes = putStrLn . show . runToStop 1 $ cukes
                 

part2 :: Map (Int,Int) Facing -> IO ()
part2 _ = putStrLn "part2"

day25 part args = do let filename = case args of
                                      [] -> "inputs/day25"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
