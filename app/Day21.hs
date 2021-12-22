{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TemplateHaskell, RankNTypes #-}

module Day21 where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Part (Part (Part1, Part2))

parseInput :: String -> (Int,Int)
parseInput input = let [player1position, player2position] = fmap (read . drop 2 . dropWhile (/= ':')) . lines $ input
                    in (player1position, player2position)

data Positions = Positions { _p1pos :: !Int, _p2pos :: !Int }
  deriving (Show, Eq, Ord)
makeLenses ''Positions

data Die = Die { _nextRoll :: !Int, _rollCount :: !Int }
  deriving (Show, Eq, Ord)
makeLenses ''Die

data Scores = Scores { _p1score :: !Int, _p2score :: !Int }
  deriving (Show, Eq, Ord)
makeLenses ''Scores

data Game = Game { _gamePositions :: !Positions, _gameDie :: !Die, _gameScores :: !Scores }
  deriving (Show, Eq, Ord)
makeFields ''Game

data Player = Player1 | Player2
  deriving (Show, Eq, Ord)

data NGame = NGame { _nGamePositions :: !Positions, _nGameScores :: !Scores, _currentPlayer :: !Player }
  deriving (Show, Eq, Ord)
makeFields ''NGame
makeLenses ''NGame


modulo1toN m = (+ 1) . (`mod` m) . (+ (-1))

roll :: State Game (Int)
roll = do result <- use $ die . nextRoll
          die . nextRoll %= modulo1toN 100 . (+ 1)
          die . rollCount += 1
          pure result

roll3 = do a <- roll
           b <- roll
           c <- roll
           pure $ a + b + c

move :: Lens' Positions Int -> State Game ()
move l = do distance <- roll3
            positions . l %= modulo1toN 10 . (+ distance)
            pure ()

turn :: Lens' Positions Int -> Lens' Scores Int -> State Game ()
turn lp ls = do move lp
                newPosition <- use $ positions . lp
                scores . ls += newPosition
                pure ()

checkWin :: Getter Scores Int -> Getter Scores Int -> State Game Int -> State Game Int
checkWin latestScoreLens otherScoreLens continue = do
  latestScore <- use $ scores . latestScoreLens
  if latestScore >= 1000
    then do otherScore <- use $ scores . otherScoreLens
            numRolls <- use $ die . rollCount
            pure $ otherScore * numRolls
    else continue

play :: State Game Int
play = do turn p1pos p1score
          checkWin p1score p2score $ do
            turn p2pos p2score
            checkWin p2score p1score play

part1 :: (Int,Int) -> IO ()
part1 (p1start, p2start) = do let positions = Positions p1start p2start
                                  die = Die 1 0
                                  scores = Scores 0 0
                              putStrLn . show . evalState play $ Game positions die scores

part2 :: (Int,Int) -> IO ()
part2 (p1start, p2start) = do let graph = buildGraph (p1start, p2start)
                              putStrLn . show . Map.size $ graph
                              putStrLn . show . sum . map length . Map.elems $ graph
                              putStrLn . show . sum . map (sum . Map.elems) . Map.elems $ graph
                              putStrLn "444356092776315"
                              putStrLn . ("player 1 wins " ++) . show $ winUniverses Player1 graph
                              putStrLn . ("player 2 wins " ++) . show $ winUniverses Player2 graph
                              putStrLn . show . filter (\(k, v) -> 0 == (k ^. scores . p2score)) . Map.assocs $ graph

positionFor Player1 = positions . p1pos
positionFor Player2 = positions . p2pos

scoreFor Player1 = scores . p1score
scoreFor Player2 = scores . p2score

otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

wonBy player game = game ^. scoreFor player == 21

winUniverses :: Player -> Map NGame (Map NGame Integer) -> Integer
winUniverses player graph = let winStates = filter (wonBy player) $ Map.keys graph
                             in sum $ pathScore <$> winStates
  where pathScore state = case Map.lookup state graph of
                            Nothing -> 1
                            Just nextMap -> sum [ count * pathScore state'
                                                | (state', count) <- Map.assocs nextMap]
                                       

nextStates :: NGame -> [NGame]
nextStates game
  | wonBy Player1 game || wonBy Player2 game = []
  | otherwise = do dice1 <- [1,2,3]
                   dice2 <- [1,2,3]
                   dice3 <- [1,2,3]
                   let distance = dice1 + dice2 + dice3
                   let player = game ^. currentPlayer
                   let newPosition = modulo1toN 10 $ (game ^. positionFor player) + distance
                   let newScore = min 21 $ (game ^. scoreFor player) + newPosition
                   pure $ positionFor player .~ newPosition
                        $ scoreFor player .~ newScore
                        $ currentPlayer .~ otherPlayer player
                        $ game

buildGraph (p1start, p2start) = go Set.empty Map.empty [NGame (Positions p1start p2start) (Scores 0 0) Player1]
  where
    go visited graph [] = graph
    go visited graph (state:states)
      | Set.member state visited = go visited graph states
      | otherwise = let nexts = nextStates state
                     in go (Set.insert state visited) 
                           (foldr (\next graph ->
                                     Map.alter (\toNextMap -> Just . Map.alter (Just . (+ 1) . fromMaybe 0
                                                                               ) state . fromMaybe Map.empty $ toNextMap
                                               ) next graph
                                  ) graph nexts)
                           (nexts ++ states)
                                              
                

day21 part args = do let filename = case args of
                                      [] -> "inputs/day21"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
