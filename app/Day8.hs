{-# LANGUAGE FlexibleContexts #-}
module Day8 (day8) where
import Part (Part (Part1, Part2))

import Control.Monad
import qualified Data.Map as Map
import Text.Parsec
import Data.Maybe (fromJust)
import Data.List (isSuffixOf)

data LR = LR_L | LR_R
  deriving (Show,Ord,Eq)

type Parsed = ([LR], [(String, (String, String))])

parsePlace = count 3 anyChar

parseMap = do lrs <- manyTill ((char 'L' >> pure LR_L) <|> (char 'R' >> pure LR_R)) newline
              newline
              places <- manyTill (do start <- parsePlace
                                     string " = ("
                                     left <- parsePlace
                                     string ", "
                                     right <- parsePlace
                                     string ")"
                                     newline
                                     pure (start, (left, right))
                                 ) eof
              pure (lrs, places)




parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseMap () "none"

stepsTo dest (lr:lrs) current places
  | dest == current = 0
  | otherwise = 1 + stepsTo dest lrs (next lr $ fromJust $ Map.lookup current places)  places

next LR_L = fst
next LR_R = snd


part1 :: Parsed -> IO ()
part1 (lrs, places) = print . stepsTo "ZZZ" (cycle lrs) "AAA" . Map.fromList $ places


findLoops lrs start places = go 0 (cycle $ zip [0..] lrs) Map.empty start
  where
    go n (ilr@(_, lr) : lrs) v p = case Map.lookup (ilr, p) v of
      Just m ->
        ( n, m,
          [ n
          | ((_, p),n) <- Map.toList v
          , "Z" `isSuffixOf` p
          ]
        )
      Nothing ->
        let p' = next lr $ fromJust $ Map.lookup p places
            v' = Map.insert (ilr, p) n v
         in go (n + 1) lrs v' p'

-- (5, 2, 4) 
-- 01234523452345
--     X   X   X 
-- (5, 2, 3)
-- 01234523452345
--    X   X   X  

targetTimes :: (Integer, Integer, [Integer]) -> [Integer]
targetTimes (returnToStart, firstAtStart, targets) = go firstAtStart
                                                        (returnToStart - firstAtStart)
                                                        (map (\target -> target - firstAtStart) targets)
  where go t0 tmax ttarget = ((+t0) <$> ttarget) ++ go (t0 + tmax) tmax ttarget


part2 :: Parsed -> IO ()
part2 (lrs, places) = let placeMap = Map.fromList places
                          starts = [p | (p,_) <- places, "A" `isSuffixOf` p]
                       in do print $ factorize 277
                             print $ factorize 300
                             targets <- forM starts $ \p -> do
                                          let loops = findLoops lrs p placeMap
                                          let times = targetTimes loops
                                          putStrLn $ show p ++ " " ++ show (findLoops lrs p placeMap) ++ " " ++ show (take 10 times)
                                          pure $ head times
                             print $ product . fmap (uncurry (^)) . Map.toList $ Map.unionsWith max (Map.fromList <$> (factorize <$> targets))

factorize m = go [] 2 0 m
  where go ls f c m | m == 1         = (f,c):ls
                    | m `mod` f == 0 = go ls f (c+1) (m `div` f)
                    | c == 0         = go ls (f+1) 0 m
                    | otherwise      = go ((f,c):ls) (f+1) 0 m


day8 part args = do let filename = case args of
                                     [] -> "inputs/day8"
                                     [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
