module Day18 (day18) where
import Part (Part (Part1, Part2))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Control.Monad (forM, void)
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Debug.Trace (trace)

type Parsed = [(Char, Integer, String)]

parseInput :: String -> Parsed
parseInput = fmap parseLine . lines

parseLine line = let [[direction], numString, charCodeString] = splitOn " " line
                  in (direction, read numString, take 6 . drop 2 $ charCodeString)

move (r,c) 'L' dist = (r,c-dist)
move (r,c) 'U' dist = (r-dist,c)
move (r,c) 'R' dist = (r,c+dist)
move (r,c) 'D' dist = (r+dist,c)

drawPoly _ [] = []
drawPoly p ((dir,dist,_):commands)
 = let p' = move p dir dist
    in (p,p'):drawPoly p' commands

polyBounds poly = let ps = [p | (start,end) <- poly, p <- [start,end]]
                      rs = fst <$> ps
                      cs = snd <$> ps
                   in ((minimum rs, maximum rs),(minimum cs, maximum cs + 10))


printPoly ((rmin,rmax),(cmin,cmax)) ps = forM [rmin..rmax] $ \r -> do
                                           forM [(cmin-1)..cmax] $ \c -> do
                                             putStr . pure $ case intersectingEdges (r,c) ps of
                                                               [] -> '.'
                                                               _ -> '#'
                                           putStr $ show r ++ "\n"



printCounted ((rmin,rmax),(cmin,cmax)) ps = let mps = Map.fromList ps
                                             in forM [rmin..rmax] $ \r -> do
                                                  forM [(cmin-1)..cmax] $ \c -> do
                                                    putStr . pure . fromJust $ Map.lookup (r,c) mps
                                                  putStr "\n"

part1 :: Parsed -> IO ()
part1 commands = let poly = drawPoly (0,0) commands
                     bounds = polyBounds poly
                  in do print poly
                        printPoly bounds poly
                        printCounted bounds (countPoints bounds poly)
                        print . length . filter (\(_,x) -> x /= '.') $ countPoints bounds poly
                        print $ countPoints2 bounds poly

intersectingEdges (r,c) poly = [edge | edge@((r0,c0), (r1,c1)) <- poly
                                     , r0 == r1 && r == r0 && inR c c0 c1 ||
                                       c0 == c1 && c == c0 && inR r r0 r1]

intersectingRow r poly = [edge | edge@((r0,c0), (r1,c1)) <- poly
                               , inR r r0 r1
                         ]



inR n n0 n1 | n0 <= n1 = n >= n0 && n <= n1
            | otherwise = inR n n1 n0

countPoints :: ((Integer, Integer), (Integer, Integer)) -> [((Integer, Integer), (Integer, Integer))] -> [((Integer,Integer),Char)]
countPoints ((rmin,rmax),(cmin,cmax)) poly = concat $ goR <$> [rmin..rmax]
  where goR rowNum = goC rowNum (cmin-1) 0 Nothing
        goC rowNum colNum edgesSoFar entryDirection
          | colNum >= cmax + 1 = []
          | otherwise
          = let es = intersectingEdges (rowNum,colNum) poly
                esH = filter (\((r0,c0),(r1,c1)) -> r0 == r1) es
                esV = filter (\((r0,c0),(r1,c1)) -> c0 == c1) es
             in case (es, esH, esV,entryDirection) of
                  ([],_,_,_) -> ((rowNum,colNum),if even edgesSoFar then '.' else '#'):goC rowNum (colNum + 1) edgesSoFar Nothing
                  (_,_,[e],Nothing) -> ((rowNum,colNum),'|'):goC rowNum (colNum + 1) (edgesSoFar + 1) (Just $ verticalDirection e)
                  (_,_,[e],Just vdir) -> ((rowNum,colNum),'|'):goC rowNum (colNum + 1) (edgesSoFar + if verticalDirection e == vdir then 0 else 1) Nothing
                  (_,[_],_,_) -> ((rowNum,colNum),'-'):goC rowNum (colNum + 1) edgesSoFar entryDirection
                  q -> error ("I dunno" ++ show q)

verticalDirection ((r0,_),(r1,_)) = compare r0 r1

organiseSegments = map (\((r0,c0),(r1,c1)) -> if c0 > c1 then ((r1,c1), (r0,c0)) else ((r0,c0), (r1,c1)))

polyS :: [((Integer,Integer),(Integer,Integer))] -> String
polyS = concatMap (\((r0,c0),(r1,c1)) -> "\n    (" ++ show r0 ++ "," ++ show c0 ++ ") -> (" ++ show r1 ++ "," ++ show c1 ++ ")" ++ if r0 == r1 then "[horizontal@row=" ++ show r0 ++ ", " ++ show c0 ++ "-" ++ show c1 ++ "]" else "[vertical@col=" ++ show c0 ++ ", " ++ show r0 ++ "-" ++ show r1 ++ "]")

countPoints2 :: ((Integer, Integer), (Integer, Integer)) -> [((Integer, Integer), (Integer, Integer))] -> Integer
countPoints2 ((rmin,rmax),(cmin,cmax)) poly = goR rmin
  where goR rowNum = trace ("Row: " ++ show rowNum ++ ", " ++ polyS (intersectingRow rowNum poly)) $
                     case organiseSegments $ intersectingRow rowNum poly of
                       [] -> 0
                       es -> let esH = filter (\((r0,c0),(r1,c1)) -> r0 == r1) es
                                 esV = filter (\((r0,c0),(r1,c1)) -> c0 == c1) es
                                 thisRow = goCs rowNum Nothing (sortBy (\((_,c0),(_,_)) ((_,c1),(_,_)) -> compare c0 c1) esV)
                                 nextInterestingRow = case esH of
                                                        [] -> minimum [r | ((r0,_),(r1,_)) <- poly, r <- [r0,r1], r > rowNum]
                                                        _ -> rowNum + 1
                                 segmentSize = nextInterestingRow - rowNum
                              in trace ("  this: " ++ show thisRow ++ ", next interesting is " ++ show nextInterestingRow ++ ", " ++ " multiplying by " ++ show segmentSize) $
                                 thisRow * segmentSize + goR nextInterestingRow
        goCs :: Integer -> Maybe Integer -> [((Integer,Integer),(Integer,Integer))] -> Integer
        goCs rowNum Nothing [] = 0
        goCs rowNum onSince ((vp0@(vr0,vc),vp1@(vr1,_)):esVs)
          | rowNum == vr0 || rowNum == vr1 = let startDir = if rowNum == vr0 then compare vr1 rowNum else compare vr0 rowNum
                                                 (vp2@(vr2,vc2),vp3@(vr3,_)) = head esVs
                                                 endDir = if rowNum == vr2 then compare rowNum vr3 else compare rowNum vr2
                                              in if startDir == endDir
                                                   then
                                                     case onSince of
                                                       --    0123456789
                                                       --    |    |   
                                                       --    |    -----    vc2 = 9, vc0 = 0
                                                       --    |        |
                                                       Just vc0 -> trace ("  A: " ++ show vc0 ++ "->" ++ show vc2 ++ "=" ++ show (vc2 - vc0 + 1)) $ (vc2 - vc0) + 1 + goCs rowNum Nothing (tail esVs)


                                                       --    | |  |   
                                                       --    | |  -----    vc = 5
                                                       --    | |      |
                                                       Nothing -> trace ("  B, saving " ++ show vc) $ goCs rowNum (Just vc) (tail esVs)
                                                   else
                                                     case onSince of
                                                       --    | |  |   |
                                                       --    | |  -----    vc2 = 9, vc = 5
                                                       --    | |      
                                                       Nothing -> trace ("  C: " ++ show vc ++ "->" ++ show vc2 ++ "=" ++ show (vc2 - vc + 1)) $ (vc2 - vc) + 1 + goCs rowNum Nothing (tail esVs)

                                                       --    |    |   |
                                                       --    |    -----    vc = 0
                                                       --    |        
                                                       _ -> trace "  D" $ goCs rowNum onSince (tail esVs)
          | otherwise = case onSince of
                          Just vc0 -> trace ("  E: " ++ show vc0 ++ "->" ++ show vc ++ "=" ++ show ((vc - vc0) + 1)) $ (vc - vc0) + 1 + goCs rowNum Nothing esVs
                          Nothing -> goCs rowNum (Just vc) esVs


commandsFromColours = fmap (\(_,_,c) -> commandFromColour c)
commandFromColour c = let hexDistance = take 5 c
                       in (case drop 5 c of
                             "0" -> 'R'
                             "1" -> 'D'
                             "2" -> 'L'
                             "3" -> 'U'
                          ,read $ "0x" ++ hexDistance
                          ,c)

printScaledPoly ((rScaleMin,rScaleMax),(cScaleMin,cScaleMax)) ((rMin,rMax),(cMin,cMax)) poly
  = let scaleC c = cScaleMin + (cScaleMax - cScaleMin) * (c - cMin) `div` (cMax - cMin) 
        scaleR r = rScaleMin + (rScaleMax - rScaleMin) * (r - rMin) `div` (rMax - rMin) 
     in do --print . length . filter (\(_,x) -> x /= '.') $ countPoints ((rScaleMin,rScaleMax),(cScaleMin,cScaleMax)) [((scaleR r0, scaleC c0),(scaleR r1, scaleC c1))|((r0,c0),(r1,c1)) <- poly]
           --print $ countPoints2 ((rScaleMin,rScaleMax),(cScaleMin,cScaleMax)) [((scaleR r0, scaleC c0),(scaleR r1, scaleC c1))|((r0,c0),(r1,c1)) <- poly]
           printPoly ((rScaleMin,rScaleMax),(cScaleMin,cScaleMax)) [((scaleR r0, scaleC c0),(scaleR r1, scaleC c1))|((r0,c0),(r1,c1)) <- poly]

part2 :: Parsed -> IO ()
part2 commands = let poly = drawPoly (0,0) . commandsFromColours $ commands
                     bounds = polyBounds poly
                  in do void $ printScaledPoly ((0,300),(0,300)) bounds poly
                        print $ countPoints2 bounds poly


day18 part args = do let filename = case args of
                                      [] -> "inputs/day18"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
