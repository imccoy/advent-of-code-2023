{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
module Day16 (day16) where
import Part (Part (Part1, Part2))
import Control.Zipper ((:>>), Top, upward, rightward, jerkTo, downward, within, fromWithin, leftward, zipper, focus, ifromWithin)
import Control.Zipper.Internal (focalPoint)
import Control.Lens
import qualified Data.Set as Set
import Data.Maybe (fromJust, catMaybes, maybeToList, mapMaybe)
import Data.Array (listArray, Array, assocs, (!), bounds, inRange)
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Debug.Trace (trace)

mkArray l = listArray (0,length l-1) l

type Parsed = [String]

parseInput :: String -> Parsed
parseInput = lines

type Z = (Top :>> Array Int (Array Int Char) :>> Array Int Char :>> Char)

moveDown :: Z -> Maybe Z
moveDown z = let p = focalPoint z
              in z & upward & rightward <&> fromWithin traversed >>= jerkTo p
moveUp :: Z -> Maybe Z
moveUp z = let p = focalPoint z
              in z & upward & leftward <&> fromWithin traversed >>= jerkTo p

position :: Z -> (Int, Int)
position z = (focalPoint (upward z), focalPoint z)

data Direction = North | East | South | West
  deriving (Eq,Ord,Show)


part1 :: Parsed -> IO ()
part1 plan = do print $ visitAllWithZ plan
                print . Set.size $ visit (mkArray . fmap mkArray $ plan) Set.empty [((0,0), East)]

visitAllWithZ plan = let start = (East, zipper (mkArray . fmap mkArray $ plan) & fromWithin traversed & fromWithin traversed)
                         visited = visitZ Set.empty [start]
                      in Set.size visited


visitZ :: Set.Set (Direction,(Int,Int)) -> [(Direction, Z)] -> Set.Set (Int,Int)
visitZ visited [] = Set.map snd visited
visitZ visited ((d,z):nexts)
  | (d,position z) `Set.member` visited = visitZ visited nexts
  | otherwise
  = let n' = case (d, view focus z) of
               (East, '|') -> [(North,) <$> moveUp z, (South,) <$> moveDown z]
               (East, '/') -> [(North,) <$> moveUp z]
               (East, '\\') -> [(South,) <$> moveDown z]
               (East, _) -> [(East,) <$> rightward z]
               (West, '|') -> [(North,) <$> moveUp z, (South,) <$> moveDown z]
               (West, '/') -> [(South,) <$> moveDown z]
               (West, '\\') -> [(North,) <$> moveUp z]
               (West, _) -> [(West,) <$> leftward z]
               (North, '-') -> [(East,) <$> rightward z, (West,) <$> leftward z]
               (North, '/') -> [(East,) <$> rightward z]
               (North, '\\') -> [(West,) <$> leftward z]
               (North, _) -> [(North,) <$> moveUp z]
               (South, '-') -> [(East,) <$> rightward z, (West,) <$> leftward z]
               (South, '/') -> [(West,) <$> leftward z]
               (South, '\\') -> [(East,) <$> rightward z]
               (South, _) -> [(South,) <$> moveDown z]
     in visitZ (Set.insert (d,position z) visited) (catMaybes n' ++ nexts)


visit :: Array Int (Array Int Char) -> Set.Set (Direction,(Int,Int)) -> [((Int,Int),Direction)] -> Set.Set (Int,Int)
visit _ visited [] = Set.map snd visited
visit plan visited ((p,d):nexts)
  | (d,p) `Set.member` visited = visit plan visited nexts
  | otherwise
  = let n' = findNeighbour plan (p,d)
     in visit plan (Set.insert (d,p) visited) (catMaybes n' ++ nexts)

findNeighbour plan ((r,c),d) =
  case (d, (plan ! r) ! c) of
    (East, '|') -> [(,North) <$> offset (-1,0) (r,c), (,South) <$> offset (1,0) (r,c)]
    (East, '/') -> [(,North) <$> offset (-1,0) (r,c)]
    (East, '\\') -> [(,South) <$> offset (1,0) (r,c)]
    (East, _) -> [(,East) <$> offset (0,1) (r,c)]
    (West, '|') -> [(,North) <$> offset (-1,0) (r,c), (,South) <$> offset (1,0) (r,c)]
    (West, '/') -> [(,South) <$> offset (1,0) (r,c)]
    (West, '\\') -> [(,North) <$> offset (-1,0) (r,c)]
    (West, _) -> [(,West) <$> offset (0,-1) (r,c)]
    (North, '-') -> [(,East) <$> offset (0,1) (r,c), (,West) <$> offset (0,-1) (r,c)]
    (North, '/') -> [(,East) <$> offset (0,1) (r,c)]
    (North, '\\') -> [(,West) <$> offset (0,-1) (r,c)]
    (North, _) -> [(,North) <$> offset (-1,0) (r,c)]
    (South, '-') -> [(,East) <$> offset (0,1) (r,c), (,West) <$> offset (0,-1) (r,c)]
    (South, '/') -> [(,West) <$> offset (0,-1) (r,c)]
    (South, '\\') -> [(,East) <$> offset (0,1) (r,c)]
    (South, _) -> [(,South) <$> offset (1,0) (r,c)]
  where
    offset (rO,cO) (r,c) = do let (r',c') = (r + rO, c + cO)
                              r'' <- if inRange (bounds plan) r' then Just r' else Nothing
                              c'' <- if inRange (bounds (plan ! 0)) c' then Just c' else Nothing
                              pure (r'',c'')

part2 input = do putStrLn "doing things this way seems to be nice and snappy"
                 part2A input
                 putStrLn "I did this next one first, but it is v slow for some reason"
                 part2Z input

part2A plan = do let planArr = mkArray (mkArray <$> plan)
                 let (minR, maxR) = bounds planArr
                 let (minC, maxC) = bounds (planArr ! minR)
                 let starts = ((,East) . (,minC) <$> [minR..maxR]) ++
                              ((,North) . (maxR,) <$> [minC..maxC]) ++
                              ((,West) . (,maxC) <$> [minR..maxR]) ++
                              ((,South) . (minR,) <$> [minC..maxC])
                 scores <- forM starts $ \start@(p,d) -> do
                   pure $ Set.size $ visit planArr Set.empty [(p,d)]
                 putStrLn $ "done" ++ show (maximum scores)

part2Z :: Parsed -> IO ()
part2Z plan = do let starts = allStarts (zipper (mkArray . fmap mkArray $ plan) & fromWithin traverse & fromWithin traverse)
                 scores <- forM starts $ \start@(d,p) -> do
                   pure $ Set.size $ visitZ Set.empty [start]
                 putStrLn $ "done" ++ show (maximum scores)
  where allStarts z = allStartsE z (moveDown z)
        allStartsE last Nothing = allStartsN last (Just last)
        allStartsE _ (Just here) = (East,here):allStartsE here (moveDown here)
        allStartsN last Nothing = allStartsW last (Just last)
        allStartsN _ (Just here) = (North,here):allStartsN here (rightward here)
        allStartsW last Nothing = allStartsS last (Just last)
        allStartsW _ (Just here) = (West,here):allStartsW here (moveUp here)
        allStartsS last Nothing = []
        allStartsS _ (Just here) = (West,here):allStartsS here (leftward here)

day16 part args = do let filename = case args of
                                      [] -> "inputs/day16"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
