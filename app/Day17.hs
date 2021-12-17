{-# LANGUAGE LambdaCase #-}

module Day17 (day17) where

import Part (Part (Part1, Part2))

parseInput :: String -> ((Int,Int),(Int,Int))
parseInput _ = ((185,221),(-122,-74))

step ((x,y),(xd,yd)) = ((x + xd, y + yd), (stepXd xd, stepYd yd))
  where stepYd yd = yd - 1
        stepXd xd | xd < 0     = xd + 1
                  | xd > 0     = xd - 1
                  | otherwise = xd

stateHeight ((_,y),_) = y

targetMaxX ((_,maxX),_) = maxX

onTarget ((minX,maxX),(minY,maxY)) ((x,y),_) = x >= minX && x <= maxX && y >= minY && y <= maxY

missedTarget ((_, maxX),(minY, _)) ((x,y),_) = x > maxX || y < minY

runTrial target = go 0
  where
    go maxHeight state | onTarget target state = Just maxHeight
                       | missedTarget target state = Nothing
                       | otherwise = let state' = step state
                                      in go (max (stateHeight state') maxHeight) state'

part1 :: ((Int,Int),(Int,Int)) -> IO ()
part1 target = tryYd 0 0
  where
    tryYd yd bestSoFar = do
      let bestSoFar' = tryXds yd (targetMaxX target) bestSoFar
      if bestSoFar' /= bestSoFar
        then putStrLn $ "Yep! " ++ show yd ++ " -> " ++ show bestSoFar'
        else putStrLn $ "Nope: " ++ show yd ++ ", still " ++ show bestSoFar 
      tryYd (yd + 1) bestSoFar'
    tryXds yd xd bestSoFar = do let bestSoFar' = case runTrial target ((0,0),(xd,yd)) of
                                                   Just maxHeight -> if maxHeight > bestSoFar
                                                                       then maxHeight
                                                                       else bestSoFar
                                                   Nothing -> bestSoFar
                                if xd == 0 then bestSoFar' else tryXds yd (xd - 1) bestSoFar'

part2 :: ((Int,Int),(Int,Int)) -> IO ()
part2 _ = putStrLn "part2"

day17 part args = do let filename = case args of
                                      [] -> "inputs/day17"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
