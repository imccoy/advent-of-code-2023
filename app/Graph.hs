{-# LANGUAGE TupleSections, ScopedTypeVariables  #-}
module Graph (invertGraph, dijkstra, dijkstraAny, dijkstraAll) where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

invertGraph :: (Ord k) => Map k [(Int, k)] -> Map k [(Int, k)]
invertGraph = Map.unionsWith (++) . concat . map (\(from, edges) -> map (\(cost, to) -> Map.singleton to [(cost, from)]) edges) . Map.toList

dijkstra graph start end = dijkstraAny graph start (\x -> x == end)


dijkstraAny :: forall k. (Eq k, Ord k, Show k) => Map k [(Int, k)] -> k -> (k -> Bool) -> Int
dijkstraAny graph start stopHere = dijkstraAll graph start (\k _ costs -> if stopHere k then Just $ costs ! k else Nothing) (error "stopHere never reached")

dijkstraAll :: forall k r. (Eq k, Ord k, Show k) => Map k [(Int, k)] -> k -> (k -> Set k -> Map k Int -> Maybe r) -> (Map k Int -> r) -> r
dijkstraAll graph start earlyHalt allExplored =
  let initialCosts = const maxBound <$> graph
      initialHeap = Map.unionsWith (<>) $ (\x -> Map.singleton maxBound [x]) <$> Map.keys graph
      pullLowCostNode :: Map Int [k] -> Maybe (k, Map Int [k])
      pullLowCostNode map = flip fmap (Map.minViewWithKey map) $
                              \((minCost, (p:restPoints)), rest) -> 
                                (p, case restPoints of
                                      [] -> rest
                                      _ -> Map.insert minCost restPoints rest)
      updateCostsHeap :: (Map k Int, Map Int [k]) -> k -> Int -> (Map k Int, Map Int [k])
      updateCostsHeap (costs, heap) point cost = ( Map.insert point cost costs
                                                 , Map.alter ((Just . (point:)) . fromMaybe []) cost heap
                                                     -- in theory we shoud delete the point from the old cost at `heap ! (costs ! point)`
                                                     -- at this point, but it's not worth it, and it's not actually necessary
                                                 )
      next (costs, heap) visited = case pullLowCostNode heap of
                                     Just (next, heap') -> go next (costs, heap') visited
                                     Nothing -> allExplored costs

      go :: k -> (Map k Int, Map Int [k]) -> Set k -> r
      go node (costs, heap) visited = 
        case earlyHalt node visited costs of 
          Just x -> x
          _ -> if Set.member node visited 
                 then next (costs, heap) visited
                 else let costToHere = fromMaybe (error $ show node ++ " not found in costs")  (Map.lookup node costs)
                          (costs', heap') = foldr (\(c, n) ch -> updateCostsHeap ch n c) (costs, heap) .
                                              mapMaybe (\(c, n) -> if costToHere + c < fromMaybe (error $ show n ++ " not found in costs 2") (Map.lookup n costs) then Just (costToHere + c, n) else Nothing) $
                                              fromMaybe (error $ show node ++ " has no edges") (Map.lookup node graph)
                       in next (costs', heap') (Set.insert node visited)
   in go start (updateCostsHeap (initialCosts,  initialHeap) start 0) Set.empty
