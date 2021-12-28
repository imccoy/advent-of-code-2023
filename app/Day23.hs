{-# LANGUAGE TupleSections #-}
module Day23 (day23) where

import Control.Monad (forM_)
import Data.Either (partitionEithers)
import Data.Foldable (null)
import Data.List (partition, sort)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

import Part (Part (Part1, Part2))

-- #############
-- #01234567890#
-- ###1#1#1#1###
--   #2#2#2#2#
--   #########
--    A B C D
--

data BurrowPosition = Corridor !Int -- 0-10
                    | SideRoom !AmphipodType !Int -- 1-2
  deriving (Show, Eq, Ord)

data AmphipodType = Amber | Bronze | Copper | Desert
  deriving (Show, Eq, Ord)

data Amphipod = Amphipod !Int !AmphipodType !BurrowPosition
  deriving (Show, Eq, Ord)

amphipodType (Amphipod _ ty _) = ty
amphipodPosition (Amphipod _ _ position) = position

data Burrow = Burrow !Int [Amphipod]
  deriving (Show, Eq, Ord)

homeColumnFromType Amber = 2
homeColumnFromType Bronze = 4
homeColumnFromType Copper = 6
homeColumnFromType Desert = 8

homeColumnFromPosition 2 = Just Amber
homeColumnFromPosition 4 = Just Bronze
homeColumnFromPosition 6 = Just Copper
homeColumnFromPosition 8 = Just Desert
homeColumnFromPosition _ = Nothing

burrowPosition (Amphipod _ _ position) = position

parseInput :: String -> [Amphipod]
parseInput _ = [ Amphipod 0 Copper (SideRoom Amber 1)
               , Amphipod 1 Desert (SideRoom Amber 2)
               , Amphipod 2 Copper (SideRoom Bronze 1)
               , Amphipod 3 Amber (SideRoom Bronze 2)
               , Amphipod 4 Bronze (SideRoom Copper 1)
               , Amphipod 5 Bronze (SideRoom Copper 2)
               , Amphipod 6 Desert (SideRoom Desert 1)
               , Amphipod 7 Amber (SideRoom Desert 2)
               ]
part2inputs = [ Amphipod 8 Desert (SideRoom Amber 2)
              , Amphipod 9 Desert (SideRoom Amber 3)
              , Amphipod 10 Copper (SideRoom Bronze 2)
              , Amphipod 11 Bronze (SideRoom Bronze 3)
              , Amphipod 12 Bronze (SideRoom Copper 2)
              , Amphipod 13 Amber (SideRoom Copper 3)
              , Amphipod 14 Amber (SideRoom Desert 2)
              , Amphipod 15 Copper (SideRoom Desert 3)
              ]

{-
parseInput _ = [ Amphipod 0 Bronze (SideRoom Amber 1)
               , Amphipod 1 Amber (SideRoom Amber 2)
               , Amphipod 2 Copper (SideRoom Bronze 1)
               , Amphipod 3 Desert (SideRoom Bronze 2)
               , Amphipod 4 Bronze (SideRoom Copper 1)
               , Amphipod 5 Copper (SideRoom Copper 2)
               , Amphipod 6 Desert (SideRoom Desert 1)
               , Amphipod 7 Amber (SideRoom Desert 2)
               ]
-}


moves burrow@(Burrow _ pods) = concat $ movesFor burrow <$> pods

movesFor burrow (Amphipod n amphipodType burrowPosition) = (n,) <$> case burrowPosition of 
                                                                      SideRoom _ _ -> movesFromSideroom burrow amphipodType burrowPosition
                                                                      Corridor _ -> movesFromCorridor burrow amphipodType burrowPosition

movesFromSideroom (Burrow _ pods) ty (SideRoom homeType depth) = if blocked depth then [] else corridorPositionsFromSideRoom (homeColumnFromType homeType)
  where blocked 0 = False
        blocked n = if locationFree pods $ SideRoom homeType (n - 1)
                      then blocked (n - 1)
                      else True
        corridorPositionsFromSideRoom n 
          | locationFree pods (Corridor $ homeColumnFromType homeType) = corridorPositionsFrom (-1) n ++ corridorPositionsFrom 1 n
          | otherwise                                                  = []
        corridorPositionsFrom adj n = let n' = n + adj
                                       in if n' >= 0 && n' <= 10 && locationFree pods (Corridor n')
                                            then case homeColumnFromPosition n' of
                                                   Nothing -> (Corridor n'):(corridorPositionsFrom adj n')
                                                   Just _ -> corridorPositionsFrom adj n'
                                            else []

locationFree pods location = Nothing == amphipodAt pods location
amphipodAt pods location = case filter ((== location) . burrowPosition) pods of
                             [] -> Nothing
                             (a:_) -> Just a
amphipodsWithRow pods n = filter (\pod -> case burrowPosition pod of
                                            SideRoom _ n' -> n == n'
                                            _ -> False
                                 ) pods

movesFromCorridor (Burrow burrowDepth pods) ty (Corridor n)
  | not $ canReach pods (homeColumnFromType ty) n                             = []
  | otherwise                                                                 = lastFree 0
  where lastFree depth = if depth == burrowDepth
                           then [SideRoom ty depth]
                           else case amphipodAt pods (SideRoom ty $ depth + 1) of
                                  Just pod -> if amphipodType pod == ty && depth > 0
                                                then [SideRoom ty $ depth]
                                                else []
                                  Nothing -> lastFree $ depth + 1

canReach :: [Amphipod] -> Int -> Int -> Bool
canReach pods end start | end < start = go (-1) start
                        | end > start = go 1 start
                        | otherwise   = False
  where go adj current | current == end = True
                       | locationFree pods (Corridor $ (current + adj)) = go adj (current + adj)
                       | otherwise = False

distance (SideRoom amphipodType depth) (Corridor position) = depth + abs (position - homeColumnFromType amphipodType)
distance (Corridor position) (SideRoom amphipodType depth) = depth + abs (position - homeColumnFromType amphipodType)
distance (Corridor position) (Corridor position') = abs (position - position')

weight Amber = 1
weight Bronze = 10
weight Copper = 100
weight Desert = 1000

costFor :: BurrowPosition -> BurrowPosition -> AmphipodType -> Int
costFor start end amphipodType = fromIntegral $ distance start end * weight amphipodType

doneState (Burrow _ pods) = go pods
  where go [] = True
        go ((Amphipod _ _ (Corridor _)):_) = False
        go ((Amphipod _ amphipodType (SideRoom roomType _)):amphipods) = amphipodType == roomType && go amphipods

applyMove (Burrow depth pods) (amphipodId, newLocation)
  = let ([Amphipod _ amphipodType currentLocation],pods') = partition (\(Amphipod n _ _) -> n == amphipodId) $ pods
     in (costFor currentLocation newLocation amphipodType, Burrow depth . sort $ (Amphipod amphipodId amphipodType newLocation):pods')

estimate (Burrow _ pods) = sum (estimateTy <$> [Amber, Bronze, Copper, Desert])
  where estimateTy ty = weight ty * sum (zipWith (+) [1..] (costsToColTop ty))
        costsToColTop ty = map (distance (Corridor $ homeColumnFromType ty) . amphipodPosition) .
                             filter (\pod -> case amphipodPosition pod of
                                               (SideRoom podTy _) -> podTy /= ty -- not already home?
                                               _ -> True
                                    ) .
                             filter ((== ty) . amphipodType) $ pods

searchFrom :: Map Int [(Int, Burrow)] -> Set Burrow -> Int
searchFrom burrowsByCost visited = let (cheapCost, cheap, burrowsByCost') = pickOne burrowsByCost
                                    in if doneState cheap
                                         then cheapCost
                                         else (if Set.size visited `mod` 100 == 0 then trace (show (Set.size visited) ++ "\n" ++ burrowString cheap) else id) $
                                              if Set.member cheap visited
                                                then searchFrom burrowsByCost' visited
                                                else searchFrom (foldr (addOne cheapCost) burrowsByCost' (applyMove cheap <$> moves cheap))
                                                                (Set.insert cheap visited)
  where
    pickOne :: Map Int [(Int, Burrow)] -> (Int, Burrow, Map Int [(Int, Burrow)])
    pickOne burrowsByCost = let ((estimate, burrows), rest) = Map.deleteFindMin burrowsByCost
                             in case burrows of
                                  ((cost, burrow):burrows) -> (cost, burrow, Map.insert estimate burrows rest)
                                  [] -> pickOne rest

    addOne :: Int -> (Int, Burrow) -> Map Int [(Int, Burrow)] -> Map Int [(Int, Burrow)]
    addOne cheapCost (cost, burrow) = Map.alter (Just . ((cheapCost + cost, burrow):) . fromMaybe []) (cheapCost + cost + estimate burrow)

amphipodLetter (Amphipod _ ty _) = case ty of
                                     Amber -> 'A'
                                     Bronze -> 'B'
                                     Copper -> 'C'
                                     Desert -> 'D'

burrowString (Burrow maxDepth pods)
   = unlines $ [ "#############"
               , "#" ++ map (\n -> fromMaybe '.' (amphipodLetter <$> amphipodAt pods (Corridor n))) [0..10] ++ "#"
               , "###" ++ map (\n -> case homeColumnFromPosition n of
                                      Nothing -> '#'
                                      Just ty -> fromMaybe '.' (amphipodLetter <$> amphipodAt pods (SideRoom ty 1))
                             ) [2..8] ++ "###"
               ] ++
               [ "  #" ++ map (\n -> case homeColumnFromPosition n of
                                      Nothing -> '#'
                                      Just ty -> fromMaybe '.' (amphipodLetter <$> amphipodAt pods (SideRoom ty depth))
                             ) [2..8] ++ "#"
               | depth <- [2..maxDepth]
               ] ++ [
                 "  ########"
               ]

printBurrow = putStrLn . burrowString

part1 :: [Amphipod] -> IO ()
part1 inputs = do let burrow = Burrow 2 inputs
                  printBurrow burrow
                  forM_ (moves burrow) $ \move -> do
                    putStrLn . show $ move
                    putStrLn . show . applyMove burrow $ move
                  putStrLn . show $ searchFrom (Map.singleton 0 [(0,burrow)]) Set.empty

withInitialMoves (move:moves) burrow = let (cost, burrow') = applyMove burrow move
                                           (costs, burrow'') = withInitialMoves moves burrow'
                                        in (cost + costs, burrow'')
withInitialMoves [] burrow = (0, burrow)

part2 :: [Amphipod] -> IO ()
part2 inputs = do let burrow = Burrow 4 $ amphipodsWithRow inputs 1 ++ 
                                          map (\(Amphipod id ty (SideRoom roomTy _)) -> Amphipod id ty (SideRoom roomTy 4))
                                              (amphipodsWithRow inputs 2) ++ 
                                          part2inputs
                  printBurrow burrow
                  putStrLn . show $ searchFrom (Map.singleton 0 [(0,burrow)]) Set.empty


day23 part args = do let filename = case args of
                                      [] -> "inputs/day23"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
