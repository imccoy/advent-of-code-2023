{-# LANGUAGE FlexibleContexts #-}
module Day22 (day22) where
import Part (Part (Part1, Part2))

import qualified Data.Heap as Heap
import Control.Monad
import Text.Parsec
import Data.Maybe (fromMaybe, fromJust, maybeToList, mapMaybe)
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Debug.Trace (trace)
import qualified Data.Set as Set

type Cubes = ((Int,Int,Int),(Int,Int,Int))

type Parsed = [Cubes]

parseCuboid = do x <- read <$> many1 digit
                 char ','
                 y <- read <$> many1 digit
                 char ','
                 z <- read <$> many1 digit
                 pure (x,y,z)

parseCubes = do a <- parseCuboid
                char '~'
                b <- parseCuboid
                pure (a,b)


parseInput :: String -> Parsed
parseInput = fmap parseLine . lines
parseLine = either (error . show) id . runParser parseCubes () "none"

shapeCuboidCount ((x1,y1,z1),(x2,y2,z2))
  | x1 /= x2 = 1 + max x1 x2 - min x1 x2
  | y1 /= y2 = 1 + max y1 y2 - min y1 y2
  | z1 /= z2 = 1 + max z1 z2 - min z1 z2
  | otherwise = 1

cubesMinZ ((x1,y1,z1),(x2,y2,z2)) = min z1 z2
cubesShiftZ offset ((x1,y1,z1),(x2,y2,z2)) = ((x1,y1,z1 + offset),(x2,y2,z2 + offset))
cubeShiftZ offset (x1,y1,z1) = (x1,y1,z1 + offset)
cubesXYs ((x1,y1,_),(x2,y2,_)) = [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]
cubesZs ((_,_,z1),(_,_,z2)) = [min z1 z2 .. max z1 z2]
cubesMaxZ ((_,_,z1),(_,_,z2)) = max z1 z2
cubesCuboids ((x1,y1,z1),(x2,y2,z2)) = [(x,y,z) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2], z <- [min z1 z2..max z1 z2]]

cubesHeap :: [Cubes] -> Heap.MinPrioHeap Int Cubes
cubesHeap = foldr (\shape heap -> Heap.insert (cubesMinZ shape, shape) heap) Heap.empty

allFall cubes = go (cubesHeap cubes) Map.empty []
  where go heap zMins fallenCubes = case Heap.splitAt 1 heap of
                                      ([(_,shape)], heap') -> let xys = cubesXYs shape
                                                                  lowestAvailable = maximum [fromMaybe 0 (Map.lookup (x,y) zMins) | (x,y) <- xys]
                                                                  fallDistance = cubesMinZ shape - lowestAvailable
                                                                  fallenShape = cubesShiftZ (-fallDistance) shape
                                                                  newZMins = Map.fromList [((x, y), z) | let z = cubesMaxZ fallenShape + 1, (x, y) <- xys]
                                                               in go heap' (Map.union newZMins zMins) (fallenShape:fallenCubes)
                                      ([],heap) -> fallenCubes

printCubes cubes = do let m = Map.fromList [(cuboid,n) | (n,shape) <- zip [0..] cubes, cuboid <- cubesCuboids shape]
                          xs = nub [x | shape <- cubes, cuboid <- cubesCuboids shape, let (x,_,_) = cuboid]
                          minX = minimum xs
                          maxX = maximum xs
                          ys = nub [y | shape <- cubes, cuboid <- cubesCuboids shape, let (_,y,_) = cuboid]
                          minY = minimum ys
                          maxY = maximum ys
                          zs = nub [z | shape <- cubes, cuboid <- cubesCuboids shape, let (_,_,z) = cuboid]
                          minZ = minimum zs
                          maxZ = maximum zs
                       in do forM_ (reverse [minZ..maxZ]) $ \z -> do
                               forM_ [minX..maxX] $ \x ->
                                 case nub [v | y <- [minY..maxY], v <- maybeToList $ Map.lookup (x,y,z) m] of
                                   [] -> putStr "."
                                   [v] -> putStr $ show v
                                   _ -> putStr "?"
                               putStr "\n"
                             putStrLn "\n\n\n"
                             forM_ (reverse [minZ..maxZ]) $ \z -> do
                               forM_ [minY..maxY] $ \y ->
                                 case nub [v | x <- [minX..maxX], v <- maybeToList $ Map.lookup (x,y,z) m] of
                                   [] -> putStr "."
                                   [v] -> putStr $ show v
                                   _ -> putStr "?"
                               putStr "\n"
                             putStrLn "\n\n\n"



part1 :: Parsed -> IO ()
part1 cubes = do let allFallen = allFall cubes
                 printCubes allFallen
                 let disintegratable = countDisintegratable allFallen
                 print disintegratable
                 print $ countDisintegratable' allFallen

countDisintegratable :: [Cubes] -> Int
countDisintegratable cubes = let numberedShapes = zip [0..] cubes
                                 cubeMap = Map.fromList [(cuboid,n) | (n,shape) <- numberedShapes, cuboid <- cubesCuboids shape]
                                 numberedShapesMap = Map.fromList numberedShapes
                                 shapesIntersecting points = nub [shapeNumber
                                                                 | cuboid <- points
                                                                 , shapeNumber <- maybeToList $ Map.lookup cuboid cubeMap
                                                                 ]
                                 shapesSupportedBy shapeNumber = filter (/= shapeNumber) $ shapesIntersecting (cubeShiftZ 1 <$> cubesCuboids (fromJust $ Map.lookup shapeNumber numberedShapesMap))
                                 shapesSupporting shapeNumber = filter (/= shapeNumber) $ shapesIntersecting (cubeShiftZ (-1) <$> cubesCuboids (fromJust $ Map.lookup shapeNumber numberedShapesMap))
                              in length $ filter (\(n,_) -> let supported = shapesSupportedBy n
                                                                otherwiseUnsupported = [n' | n' <- supported, not (any (/= n) (shapesSupporting n'))]
                                                             in null otherwiseUnsupported
                                                 ) numberedShapes

countDisintegratable' = length . getDisintegratable'

getDisintegratable' cubes = let (_,supportedGraph, supportingGraph) = buildDisintegratableGraph cubes
                              in [n | (n, supporteds) <- Map.toList supportedGraph
                                    , let otherwiseUnsupported = [supported | supported <- supporteds, not . any (/= n) $ fromJust $ Map.lookup supported supportingGraph]
                                    , null otherwiseUnsupported]




buildDisintegratableGraph :: [Cubes] -> (Map.Map Int Cubes, Map.Map Int [Int], Map.Map Int [Int])
buildDisintegratableGraph cubes = let numberedShapes = zip [0..] cubes
                                      cubeMap = Map.fromList [(cuboid,n) | (n,shape) <- numberedShapes, cuboid <- cubesCuboids shape]
                                      numberedShapesMap = Map.fromList numberedShapes
                                      shapesIntersecting points = nub [shapeNumber
                                                                      | cuboid <- points
                                                                      , shapeNumber <- maybeToList $ Map.lookup cuboid cubeMap
                                                                      ]
                                      shapesOnTopOf shapeNumber = filter (/= shapeNumber) $ shapesIntersecting (cubeShiftZ 1 <$> cubesCuboids (fromJust $ Map.lookup shapeNumber numberedShapesMap))
                                      shapesUnderneath shapeNumber = filter (/= shapeNumber) $ shapesIntersecting (cubeShiftZ (-1) <$> cubesCuboids (fromJust $ Map.lookup shapeNumber numberedShapesMap))
                                   in (Map.fromList numberedShapes
                                      ,Map.fromList [(n,shapesOnTopOf n) | (n,_) <- numberedShapes]
                                      ,Map.fromList [(n,shapesUnderneath n) | (n,_) <- numberedShapes])


getChainReactions cubes = let (numberedShapes, shapesOnTop, shapesUnderneath) = buildDisintegratableGraph cubes
                              explosive = Set.toList (Map.keysSet numberedShapes `Set.difference` Set.fromList (getDisintegratable' cubes))
                              reactionSize condemned [] = condemned
                              reactionSize condemned shapeNumbers = let potentialNexts = concatMap (fromJust . (`Map.lookup` shapesOnTop)) shapeNumbers
                                                                        nexts = filter (\n ->
                                                                                          let otherSupports = [ otherSupport
                                                                                                              | otherSupport <- fromMaybe [] $ Map.lookup n shapesUnderneath
                                                                                                              , otherSupport /= n
                                                                                                              , not $ otherSupport `Set.member` condemned
                                                                                                              , otherSupport `notElem` shapeNumbers]
                                                                                          in null otherSupports
                                                                                       ) potentialNexts
                                                                     in reactionSize (Set.union condemned (Set.fromList nexts)) nexts
                           in sum $ fmap (Set.size . reactionSize Set.empty . pure) explosive

part2 :: Parsed -> IO ()
part2 cubes = do let allFallen = allFall cubes
                 printCubes allFallen
                 let (_,onTopOf,underneath) = buildDisintegratableGraph allFallen
                 forM_ (Map.toList onTopOf) $ \(k, r) ->
                   putStrLn $ show k ++ " is on top of: " ++ show r
                 forM_ (Map.toList underneath) $ \(k, r) ->
                   putStrLn $ show k ++ " is underneath: " ++ show r
                 let chainReactions = getChainReactions allFallen
                 print chainReactions

day22 part args = do let filename = case args of
                                      [] -> "inputs/day22"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
