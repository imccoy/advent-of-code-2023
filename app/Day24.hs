module Day24 (day24) where
import Part (Part (Part1, Part2))

import Text.Parsec
import Control.Monad (void, forM_, when)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Functor ((<&>))
import Data.List (sort)
import Debug.Trace (trace)

type Parsed = [((Integer,Integer,Integer),(Integer,Integer,Integer))]

parseNum = do spaces
              minus <- (char '-' >> pure True) <|> pure False
              digits <- many1 digit
              pure $ (if minus then (-1) else 1) * read digits

parseTriple = do x <- parseNum
                 string ", "
                 y <- parseNum
                 string ", "
                 z <- parseNum
                 pure (x,y,z)

parseHail = do position <- parseTriple
               string " @ "
               velocity <- parseTriple
               newline
               pure (position, velocity)

parseHailstorm = manyTill parseHail eof



-- x1 + t1 * vx1 = A
-- x2 + t2 * vx2 = A
-- y1 + t1 * vy1 = B
-- y2 + t2 * vy2 = B
--
-- t1 = (A - x1) / vx1
-- t2 = (A - x2) / vx2
-- y1 + ((A - x1) / vx1) * vy1 = B
-- y2 + ((A - x2) / vx2) * vy2 = B
-- y1 + ((A - x1) / vx1) * vy1 = y2 + ((A - x2) / vx2) * vy2
-- y1 - y2 = ((A - x2) / vx2) * vy2 - ((A - x1) / vx1) * vy1 
-- y1 - y2 = ((A - x2) / vx2) * vy2 - ((A - x1) / vx1) * vy1 
-- vx1 * vx2 * (y1 - y2) = (A - x2) * vy2 * vx1 - (A - x1) * vy1 * vx2
-- vx1 * vx2 * (y1 - y2) = A * vy2 * vx1 - x2 * vy2 * vx1 - A * vy1 * vx2 + x1 * vy1 * vx2
--
-- vx1 * vx2 * (y1 - y2) + x2 * vy2 * vx1 - x1 * vy1 * vx2 = A * vy2 * vx1 - A * vy1 * vx2
-- (vx1 * vx2 * (y1 - y2) + x2 * vy2 * vx1 - x1 * vy1 * vx2) / (vy2 * vx1 - vy1 * vx2) = A
-- 

findMeeting2 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> Maybe (Rational,Rational)
findMeeting2 (x1',y1') (x2',y2') (vx1',vy1') (vx2',vy2')
  = let (x1, y1)  = (fromIntegral x1', fromIntegral y1')
        (x2, y2)  = (fromIntegral x2', fromIntegral y2')
        (vx1,vy1) = (fromIntegral vx1', fromIntegral vy1')
        (vx2,vy2) = (fromIntegral vx2', fromIntegral vy2')
        x = (vx1 * vx2 * (y1 - y2) + x2 * vy2 * vx1 - x1 * vy1 * vx2) / (vy2 * vx1 - vy1 * vx2)
        y = y1 + ((x - x1) / vx1) * vy1
     in if vy2 * vx1 - vy1 * vx2 == 0 then Nothing else Just (x,y)


parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseHailstorm () "none"

allPotentialMeetings [] = []
allPotentialMeetings (x:xs) = go x xs
  where go x (y:ys) = (x,y):go x ys
        go _ [] = allPotentialMeetings xs

rationalToDouble :: Rational -> Double
rationalToDouble = fromRational

-- y = mx + c
--
-- x = 0 -> t = ?
-- x + t * vx = 0
-- t * vx = -x
-- t = -x / vx
-- c = y + t * vy
--   = y - x / vx * vy
-- m = rise / run = vy / vx
--
-- m1 * x + c1 = m2 * x + c2
-- m1 * x - m2 * x = c2 - c1
-- x = (c2 - c1) / (m1 - m2)
toVec2 :: (Integer,Integer) -> (Integer,Integer) -> (Rational,Rational)
toVec2 (x,y) (vx,vy) = (fromIntegral y - fromIntegral x / fromIntegral vx * fromIntegral vy, fromIntegral vy / fromIntegral vx)


part1 :: Parsed -> IO ()
part1 inputs = let (min,max) = if length inputs > 10 then (200000000000000,400000000000000) else (7,27)
                   meetingTimes = [ (t1,t2,(x,y))
                                  | (((x1,y1,_),(vx1,vy1,_)),((x2,y2,_),(vx2,vy2,_))) <- allPotentialMeetings inputs
                                  , (x,y) <- maybeToList $ findMeeting2 (x1,y1) (x2,y2) (vx1,vy1) (vx2,vy2)
                                  , let t1 = findTime x1 vx1 x
                                  , let t2 = findTime x2 vx2 x
                                  ]
               in do print . length . filter (coordsInBounds min max) $ meetingTimes
                     print meetingTimes
                     print [ (t1,t2,(x,y))
                           | (((x1,y1,_),(vx1,vy1,_)),((x2,y2,_),(vx2,vy2,_))) <- allPotentialMeetings inputs
                           , let (c1, m1) = toVec2 (x1,y1) (vx1,vy1)
                           , let (c2, m2) = toVec2 (x2,y2) (vx2,vy2)
                           , (x,y) <- if m1 /= m2
                                        then let x = (c2 - c1) / (m1 - m2)
                                                 y = m1 * x + c1
                                              in [(x,y)]
                                        else []
                           , let t1 = findTime x1 vx1 x
                           , let t2 = findTime x2 vx2 x
                           ]



-- p + v * t = p'
findTime :: Integer -> Integer -> Rational -> Rational
findTime p v p' = (p' - fromIntegral p) / fromIntegral v

coordsInBounds :: Integer -> Integer -> (Rational, Rational, (Rational, Rational)) -> Bool
coordsInBounds min max (t1,t2,(x,y))
  = t1 > 0 && t2 > 0 &&
    x >= fromIntegral min && x <= fromIntegral max &&
    y >= fromIntegral min && y <= fromIntegral max

cross (ax,ay,az) (bx,by,bz) = (ay*bz-az*by
                              ,az*bx-ax*bz
                              ,ax*by-ay*bx)

dot (ax,ay,az) (bx,by,bz) = ax*bx + ay*by + az*bz

sub3 (x1,y1,z1) (x2,y2,z2) = (x1 - x2, y1 - y2, z1 - z2)
add3 (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)
scale3 n (x,y,z) = (n * x, n * y, n * z)

score :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> [((Integer,Integer,Integer),(Integer,Integer,Integer))] -> Double
score (px1,py1,pz1) (px2,py2,pz2) lines
  = let vx0 = px2 - px1
        vy0 = py2 - py1
        vz0 = pz2 - pz1
        -- https://math.stackexchange.com/questions/2213165/find-shortest-distance-between-lines-in-3d
        score1 ((px,py,pz),(vx,vy,vz))
          = let n = cross (vx0,vy0,vz0) (vx,vy,vz)
             in if n == (0,0,0)
                  then Nothing
                  else Just $ fromIntegral (abs $ dot n (px1-px, py1-py, pz1-pz)) / sqrt (fromIntegral $ dot n n)
                  --else Just (fromRational (nx * (px1 - fromIntegral px) + ny * (py1 - fromIntegral py) + nz * (pz1 - fromIntegral pz)), sqrt (fromRational $ nx*nx + ny*ny + nz*nz))
     in sum $ fmap (fromMaybe 100000000 . score1) lines

findTs :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> (Rational, Rational)
findTs p1 v1 p2 v2 = let n = cross v1 v2
                      in (fromInteger (dot (cross v2 n) (sub3 p2 p1)) / fromInteger (dot n n)
                         ,fromInteger (dot (cross v1 n) (sub3 p1 p2)) / fromInteger (dot n n)
                         )
                         
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [a] = []
pairs (a:b:cs) = (a,b):pairs (b:cs)

accessibleT (lower,upper) p v | v < 0 = (if p < upper then 0 else (upper - p + (-v - 1)) `div` (-v)
                                        ,(p - lower) `div` (-v))
                              | v > 0 = (if p > lower then 0 else (p - lower + (v - 1)) `div` v
                                        ,(upper - p) `div` v)
accessibleT3 (lower,upper) (px,py,pz) (vx,vy,vz) = (maximum [xMinT,yMinT,zMinT]
                                                   ,minimum [xMaxT,yMaxT,zMaxT])
   where (xMinT,xMaxT) = accessibleT (lower,upper) px vx
         (yMinT,yMaxT) = accessibleT (lower,upper) py vy
         (zMinT,zMaxT) = accessibleT (lower,upper) pz vz

accessibleSlices (tMin,tMax) = [ (a + (b - a) `div` 2, (a,b))
                               | (a,b) <- if tMax - tMin > chunks
                                            then pairs $ [tMin] ++ [tMin + step * (tMax - tMin) `div` chunks | step <- [1..chunks-1]] ++ [tMax]
                                            else [(v,v) | v <- [tMin..tMax]]
                               ]
  where chunks = 100 


-- this approach is not great! It gets it right with chunks=100, but wrong with chunks=50. But it does get it right with chunks=100, so I'm calling it good
findStriker (lower,upper) (startTMin,startTMax) (((px1,py1,pz1),(vx1,vy1,vz1)):((px2,py2,pz2),(vx2,vy2,vz2)):lines)
  | startTMin == startTMax
  = let findEnd (endTMin, endTMax)
          | endTMin == endTMax
          = (px2 + vx2 * endTMin, py2 + vy2 * endTMin, pz2 + vz2 * endTMin)
          | otherwise
          = let (_,(nextMin,nextMax)) = minimum [ (score (px1+vx1 * startTMin,py1 + vy1 * startTMin,pz1 + vz1 * startTMin) (px2 + vx2 * tMid,py2 + vy2 * tMid,pz2 + vz2 * tMid) lines, (tMin,tMax))
                                                | (tMid,(tMin,tMax)) <- accessibleSlices (endTMin, endTMax)
                                                ]
             in findEnd (nextMin, nextMax)
     in ((px1+vx1 * startTMin,py1 + vy1 * startTMin,pz1 + vz1 * startTMin),findEnd $ accessibleT3 (lower,upper) (px2,py2,pz2) (vx2,vy2,vz2))

  | otherwise
  = let bestCandidateEnd n (endTMin,endTMax) startp
           = --trace (show startp ++ " -> " ++ show xlower ++ "-" ++ show xupper ++ "," ++ show ylower ++ "-" ++ show yupper ++ "," ++ show zlower ++ "-" ++ show zupper) $
             let (s,(nextMin,nextMax)) = minimum [ (score startp (px2 + vx2 * tMid,py2 + vy2 * tMid,pz2 + vz2 * tMid) lines, (tMin,tMax))
                                                 | (tMid,(tMin,tMax)) <- accessibleSlices (endTMin, endTMax)
                                                 ]
              in if n == 5 then s
                           else bestCandidateEnd (n+1) (nextMin,nextMax) startp
     in let (_,(nextMin,nextMax)) = minimum [ (bestCandidateEnd 0 (accessibleT3 (lower,upper) (px2,py2,pz2) (vx2,vy2,vz2)) (px1+vx1 * tMid,py1 + vy1 * tMid,pz1 + vz1 * tMid), (tMin,tMax))
                                            | (tMid,(tMin,tMax)) <- accessibleSlices (startTMin, startTMax)
                                            ]
         in findStriker (lower,upper) (nextMin,nextMax) (((px1,py1,pz1),(vx1,vy1,vz1)):((px2,py2,pz2),(vx2,vy2,vz2)):lines)

normVelocity (x,y,z) = (x `div` s, y `div` s, z `div` s)
  where s = gcd (gcd (abs x) (abs y)) (abs z)

part2 :: Parsed -> IO ()
part2 inputs = let (min, max) = if length inputs > 10 then (200000000000000,400000000000000) else (7,27)
                   (start,end) = if True
                                   then findStriker (min,max) (uncurry (accessibleT3 (min,max)) (head inputs)) inputs
                                        -- these numbers drawn from a run of findStriker above
                                   else ((339043905191802,283207786679997,332720221552991),(339043905191802-245,283207786679997-75,332720221552991-221))
                   velocity = normVelocity $ sub3 end start
                in do print start
                      print velocity
                      forM_ [(t1,t2) | (p,v) <- inputs, let (t1,t2) = findTs start velocity p v] $ \(t1,t2) ->
                        print (t1, t2, t2 - t1, rationalToDouble $ t2 - t1)
                      -- turns out the difference between time-to-hit to the found vector and to the supplied vector is the same for all the supplied vectors
                      let (t1,t2) = uncurry (findTs start velocity) (head inputs)
                      -- so pull back far enough that the difference between time-to-hit on the two vectors will be zero
                      let start' = sub3 start (scale3 (floor $ t2 - t1) velocity)
                      forM_ [(t1,t2) | (p,v) <- inputs, let (t1,t2) = findTs start' velocity p v] $ \(t1,t2) ->
                        print (t1, t2, t2 - t1, rationalToDouble $ t2 - t1)
                      -- and ship it
                      print $ dot start' (1,1,1)



day24 part args = do let filename = case args of
                                      [] -> "inputs/day24"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
