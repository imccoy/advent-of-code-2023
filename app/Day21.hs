{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Day21 (day21) where
import Part (Part (Part1, Part2))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Base (RuntimeRep(AddrRep))
import Data.Maybe (fromJust, fromMaybe, catMaybes, maybeToList, isJust, isNothing)
import Control.Monad (forM_, forM)
import Data.Foldable (find)
import Debug.Trace (trace)
import Data.Vector.Fusion.Bundle (Step(Yield))
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (sort, sortBy)


newtype Row = Row { unRow :: Integer }
  deriving (Show, Ord, Eq, Enum)
newtype Col = Col { unCol :: Integer }
  deriving (Show, Ord, Eq, Enum)

data Cell = Empty | Garden | Start
  deriving (Show, Ord, Eq)

shiftRow :: Integer -> Row -> Row
shiftRow o (Row n) = Row $ n + o
shiftCol :: Integer -> Col -> Col
shiftCol o (Col n) = Col $ n + o

type Parsed = (Row,Col,(Row,Col),Set.Set (Row,Col))

parseCell '#' = Empty
parseCell '.' = Garden
parseCell 'S' = Start

type RawGrid = (Row,Col,[[((Row,Col),Cell)]])
parseInput0 :: String -> RawGrid
parseInput0 input = let rowsData = lines input
                        rows = length rowsData
                        cols = case rowsData of
                                 [] -> 0
                                 (r:_) -> length r
                     in (Row $ fromIntegral rows
                        , Col $ fromIntegral cols
                        , [ [ ((Row rowNumber, Col colNumber), parseCell colData)
                            | (colNumber, colData) <- zip [0..] rowData
                            ]
                          | (rowNumber, rowData) <- zip [0..] rowsData
                          ]
                        )


parseInput :: String -> Parsed
parseInput input = let (height,width,initial) = parseInput0 input
                       start = fst . head . filter ((== Start) . snd) . concat $ initial
                       gardens = Set.fromList . map fst . filter ((\x -> x == Garden || x == Start) . snd) . concat $ initial
                    in (height,width,start, gardens)

step :: Set.Set (Row,Col) -> Set.Set (Row,Col) -> Set.Set (Row,Col)
step gardens = Set.fromList . concatMap nexts . Set.toList
  where nexts (row,col) = [loc | loc <- [(shiftRow (-1) row,col)
                                        ,(shiftRow 1 row, col)
                                        ,(row,shiftCol (-1) col)
                                        ,(row,shiftCol 1 col)]
                               , loc `Set.member` gardens]

part1 :: Parsed -> IO ()
part1 (height,width,start,gardens) = do print gardens
                                        print start
                                        print $ step gardens (Set.singleton start)
                                        print . Set.size $ gardens
                                        print . Set.size . Set.map rotate $ gardens
                                        print . Set.size . (!! 64) $ iterate (step gardens) (Set.singleton start)
                                        print . Set.size . (!! 64) $ iterate (stepR $ Set.map rotate gardens) (Set.singleton $ rotate start)
                                        let rotTest = Map.fromList [((Row r, Col c),show (r*3 + c)) | r<-[0..2],c<-[0..2]]
                                        printAtPoints $ rotTest
                                        printAtPoints $ Map.mapKeys rotate rotTest
                                        printAtPoints $ Map.mapKeys (fromJust . unrotate . rotate) rotTest
                                        print (Map.mapKeys (fromJust . unrotate . rotate) rotTest == rotTest)
                                        print (gardens == Set.map (fromJust . unrotate . rotate) gardens)
                                        printAtPoints $ Map.mapKeys rotate rotTest

megaStep :: Row -> Col -> Set.Set (Row,Col) -> Set.Set (Row,Col) -> Set.Set (Row,Col)
megaStep height width = megaStepN (\(Row r) -> Row $ r `mod` unRow height) (\(Col c) -> Col $ c `mod` unCol width)

megaStepN normalizeR normalizeC gardens = Set.fromList . concatMap nexts . Set.toList
  where nexts (row,col) = [loc | loc <- [(shiftRow (-1) row,col)
                                        ,(shiftRow 1 row, col)
                                        ,(row,shiftCol (-1) col)
                                        ,(row,shiftCol 1 col)]
                               , normalize loc `Set.member` gardens]
        normalize (r, c) = (normalizeR r, normalizeC c)


megaStepRN normalizeR normalizeC gardens = Set.fromList . concatMap nexts . Set.toList
  where nexts (row,col) = [loc | loc <- [(shiftRow (-1) row,shiftCol (-1) col)
                                        ,(shiftRow 1 row   ,shiftCol (-1) col)
                                        ,(shiftRow (-1) row,shiftCol 1 col)
                                        ,(shiftRow 1 row,shiftCol 1 col)]
                               , normalize loc `Set.member` gardens]
        normalize (r, c) = (normalizeR r, normalizeC c)




printAtPointsR ps = let ((minR,maxR),(minC,maxC)) = getBounds . Map.keys $ ps
                     in forM_ [minR..maxR] $ \r -> do
                          forM_ [minC..maxC] $ \c -> do
                            if isJust (unrotate (r,c))
                              then putStr $ fromMaybe "#" $ Map.lookup (r,c) ps
                              else putStr " "
                          putStr "\n"

printAtPoints ps = let ((minR,maxR),(minC,maxC)) = getBounds . Map.keys $ ps
                    in forM_ [minR..maxR] $ \r -> do
                         forM_ [minC..maxC] $ \c -> do
                           putStr $ fromMaybe " " $ Map.lookup (r,c) ps
                         putStr "\n"


findFixedPoint start gardens = let gs = iterate (step gardens) (Set.singleton start)
                                   (n,(g1,_)):(_,(g2,_)):_ = dropWhile (\(n,(g1,g2)) -> g1 /= g2) $ zip [0..] $ zip gs (drop 2 gs)
                                   sizeAfterOddIterations = if n `mod` 2 == 1 then Set.size g1 else Set.size g2
                                   sizeAfterEvenIterations = if n `mod` 2 == 0 then Set.size g1 else Set.size g2
                                in (n,sizeAfterOddIterations,sizeAfterEvenIterations)
--      4
--     434
--    43234
--   4321234
--  432101234
--   4321234
--    43234
--     434
--      4

analyse (height,width,start,gardens) = go 0 Nothing Map.empty Map.empty (Map.singleton (Row 0,Col 0) (Set.singleton start))
  where go :: Integer -> Maybe Integer -> Map.Map Integer [(Integer,(Row,Col))] -> Map.Map (Set.Set (Row,Col)) Integer -> Map.Map (Row,Col) (Set.Set (Row,Col)) -> (Map.Map (Set.Set (Row,Col)) Integer,Map.Map Integer [(Integer, (Row,Col))])
        go n halt tiles tileNames accessible = let accessible' = fmap Set.fromList . Map.fromListWith (++) . concatMap nexts . Map.toList $ accessible
                                                   tileNames' = foldr (\tile names -> case Map.lookup tile names of
                                                                                        Nothing -> trace "new" $ Map.insert tile (fromIntegral $ Map.size names) names
                                                                                        Just _ -> trace "existing" $ names)
                                                                      tileNames
                                                                      (Map.elems accessible')
                                                in if Just n /= halt
                                                     then trace (show n) $
                                                          go (n+1) (if isNothing halt && Map.size tileNames' == Map.size tileNames then Just $ n + 20 else halt)
                                                             (foldr (\(bigRbigC, tile) tiles' -> Map.insertWith (++) (fromJust $ Map.lookup tile tileNames') [(n,bigRbigC)] tiles')
                                                                    tiles
                                                                    (Map.toList accessible')
                                                             ) tileNames' accessible'
                                                     else (tileNames, tiles)
        nexts :: ((Row,Col),Set.Set (Row,Col)) -> [((Row,Col),[(Row,Col)])]
        nexts ((bigR,bigC),ps) = concatMap (nexts1 . ((bigR,bigC),)) (Set.toList ps)
        nexts1 ((bigR,bigC),(row,col)) = [(bigPos,[p])
                                         |(bigPos,p) <- [if row > Row 0 then ((bigR,bigC),(shiftRow (-1) row,col))               else ((shiftRow (-1) bigR,bigC              ),(maxR,col))
                                                        ,if row < maxR  then ((bigR,bigC),(shiftRow 1    row,col))               else ((shiftRow 1 bigR   ,bigC              ),(Row 0,col))
                                                        ,if col > Col 0 then ((bigR,bigC),(row,              shiftCol (-1) col)) else ((bigR              ,shiftCol (-1) bigC),(row,maxC))
                                                        ,if col < maxC  then ((bigR,bigC),(row,              shiftCol 1    col)) else ((bigR              ,shiftCol 1    bigC),(row,Col 0))
                                                        ]
                                         , p `Set.member` gardens
                                         ]
        maxR = shiftRow (-1) height
        maxC = shiftCol (-1) width

printTile gardens width height set  =
  printAtPoints $ Map.fromList [((r,c),if (r,c) `Set.member` set then "O" else (if (r,c) `Set.member` gardens then "." else "#"))
                               |r <- [Row 0..height],c<-[Col 0..width]]

part2 :: Parsed -> IO ()
part2 input@(height,width,start@(startR,startC),gardens) = do
  error "nope!!!"
  let (names, graph) = analyse input
  forM_ (sortBy (\a b -> compare (snd a) (snd b))  $ Map.toList names) $ \(set, name) -> do
    putStrLn $ show name ++ " " ++ show (fmap (bimap unRow unCol) . Set.toList $ set)
    printAtPoints $ Map.fromList [((r,c),if (r,c) `Set.member` set then "O" else (if (r,c) `Set.member` gardens then "." else "#"))
                                 |r <- [Row 0..height],c<-[Col 0..width]]
  forM_ (Map.toList graph) $ \(tile,occurrences) -> putStrLn $ show tile ++ ": " ++ show (reverse occurrences)
  forM_ [(0,0),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1)] $ \(rDir, cDir) -> do
    let rConstraint | rDir == 0 = (== Row 0)
                    | rDir < 0  = ((<0) . unRow)
                    | rDir > 0  = ((>0) . unRow)
    let cConstraint | cDir == 0 = (== Col 0)
                    | cDir < 0  = ((<0) . unCol)
                    | cDir > 0  = ((>0) . unCol)
    print (rDir, cDir)
    let tiles = [(abs (unRow r) + abs (unCol c),time,(r,c),tile) | (tile,occurrences) <- Map.toList graph, (time,(r,c)) <- occurrences, rConstraint r, cConstraint c]
    forM_ (sort tiles) $ \(distance, time, loc@(r,c), tile) -> do
      putStrLn $ "distance=" ++ show distance ++
                  "(" ++ show (bimap unRow unCol loc) ++ ")" ++
                  " time=" ++ show time ++ " " ++ show (time `mod` (unRow height + unCol width)) ++ " " ++ show (time `mod` (unRow height)) ++ " " ++
                  " tile=" ++ show tile
  let tilesByDir = Map.fromList [((dir,time - head times), tile)
                                | (dirs,times) <- [([(-1,0),(0,1),(1,0),(0,-1)],[unRow startR..unRow height * 2])
                                                  ,([(-1,1),(1,1),(1,-1),(-1,-1)],[unRow height + 1..unRow height * 4])
                                                  ,([(0,0)],[unRow startR + unRow startR..unRow startR + unRow startR + 1])
                                                  ]
                                , dir <- dirs, time <- times
                                , (tile,occurrences) <- Map.toList graph, (time,(r,c)) <- occurrences, (unRow r,unCol c) == dir, time `elem` times
                                ]
  forM_ (sort $ Map.keys tilesByDir) $ \(dir,time) ->
    putStrLn $ show dir ++ " " ++ show time ++ " " ++ show (fromJust $ Map.lookup (dir,time) tilesByDir)
  let tilesByName = Map.fromList . fmap (\(a,b) -> (b,a)) . Map.toList $ names
  let countFor n = do
        let distanceIntoLastCell = (n - unRow startR - 1) `mod` unRow height
        putStrLn $  "n=" ++ show n ++ ", distance into last cell is " ++ show distanceIntoLastCell
        corners <- fmap sum . forM [(-1,0),(0,1),(1,0),(0,-1)] $ \corner -> do
                                let tile = Map.lookup (corner, distanceIntoLastCell) tilesByDir
                                let size = fromIntegral . Set.size $ fromMaybe Set.empty (tile >>= (`Map.lookup` tilesByName))
                                putStrLn $  "corner " ++ show corner ++ " at " ++ show distanceIntoLastCell ++ " is tile " ++ show tile ++ " with size " ++ show size
                                maybe (putStrLn "blank") (printTile gardens width height . fromJust . (`Map.lookup` tilesByName)) tile
                                pure size
        let havePrecedingWingsCell = distanceIntoLastCell < (unRow height + 1) `div` 2
        wings <- if havePrecedingWingsCell
                   then fmap sum . forM [(-1,0),(0,1),(1,0),(0,-1)] $ \corner -> do
                                     let tile = fromJust $ Map.lookup (corner, unRow height + distanceIntoLastCell) tilesByDir
                                     let size = fromIntegral . Set.size $ fromJust $ Map.lookup tile tilesByName
                                     putStrLn $  "cornerWings " ++ show corner ++ " at " ++ show distanceIntoLastCell ++ " is tile " ++ show tile ++ " with size " ++ show size
                                     printTile gardens width height $ fromJust $ Map.lookup tile tilesByName
                                     pure size
                   else pure 0
        let bigRectLen = (if havePrecedingWingsCell then n - 2 else n - 1) `div` unRow height
        let bigRectTileSizes = if even bigRectLen
                                 then (fromIntegral . Set.size . fromJust $ Map.lookup ((0,0),0) tilesByDir >>= (`Map.lookup` tilesByName)
                                      ,fromIntegral . Set.size . fromJust $ Map.lookup ((0,0),1) tilesByDir >>= (`Map.lookup` tilesByName))
                                 else (fromIntegral . Set.size . fromJust $ Map.lookup ((0,0),n `mod` 2) tilesByDir >>= (`Map.lookup` tilesByName)
                                      ,fromIntegral . Set.size . fromJust $ Map.lookup ((0,0),(n+1) `mod` 2) tilesByDir >>= (`Map.lookup` tilesByName))
        let smallBigRectHalf = (bigRectLen * bigRectLen) `div` 2
        let bigBigRectHalf = (bigRectLen * bigRectLen) - smallBigRectHalf
        putStrLn $ "bigRectLen=" ++ show bigRectLen ++ " -> " ++ show bigRectTileSizes ++ " " ++ show (smallBigRectHalf, bigBigRectHalf)
        let bigRectSize = (fst bigRectTileSizes * smallBigRectHalf) + (snd bigRectTileSizes * bigBigRectHalf)
        print $ show $ corners + bigRectSize
  countFor 10
  countFor 50
  countFor 100
  print ()

-- 1            1               
-- 2   C        2               
-- 3  B F       3               
-- 4 A E J      4 ABC            
-- 5  D I       5 DEF           
-- 6   H        6 HIJ           
-- 7            7               
--  123456789    123456789               
--
rotate (Row r, Col c) = (Row (r - c), Col (r + c))
-- rotate (rotate (r,c)) = rotate (r-c,r+c) = (r-c-(r+c),r-c+(r+c)) = (-2c,2r)
unrotate (Row r, Col c) = let (r',c') = rotate (Row r, Col c)
                           in if even (unCol c') && even (unRow r') then Just (Row $ unCol c' `div` 2, Col $ unRow r' `div` (-2)) else Nothing

stepR :: Set.Set (Row,Col) -> Set.Set (Row,Col) -> Set.Set (Row,Col)
stepR gardens = Set.fromList . concatMap nexts . Set.toList
  where nexts (row,col) = [loc | loc <- [(shiftRow (-1) row, shiftCol (-1) col)
                                        ,(shiftRow 1    row, shiftCol (-1) col)
                                        ,(shiftRow (-1) row, shiftCol 1 col)
                                        ,(shiftRow 1    row, shiftCol 1 col)]
                               , loc `Set.member` gardens]

findFixedPointR start gardens = let gs = iterate (stepR gardens) (Set.singleton start)
                                    (n,(g1,_)):(_,(g2,_)):_ = dropWhile (\(n,(g1,g2)) -> g1 /= g2) $ zip [0..] $ zip gs (drop 2 gs)
                                    sizeAfterOddIterations = if n `mod` 2 == 1 then Set.size g1 else Set.size g2
                                    sizeAfterEvenIterations = if n `mod` 2 == 0 then Set.size g1 else Set.size g2
                                 in (n,sizeAfterOddIterations,sizeAfterEvenIterations)


getBounds [] = ((Row 0,Row 0),(Col 0,Col 0))
getBounds coords = let minR = minimum $ map fst coords
                       maxR = maximum $ map fst coords
                       minC = minimum $ map snd coords
                       maxC = maximum $ map snd coords
                    in ((minR,maxR),(minC,maxC))


fillOutRotatedS gardens height width = Set.fromList . Map.keys $ fillOutRotated (Map.fromList . fmap (,True) . Set.toList  $ gardens) height width
fillOutRotated gardens height width = let rotated = Map.mapKeys rotate gardens
                                          ((minR,maxR),(minC,maxC)) = getBounds $ Map.keys rotated
                                          normalize (Row r, Col c) = (Row $ r `mod` unRow height, Col $ c `mod` unCol width)
                                          cForTilable = case find (\cOff -> all (\r -> normalize (r, minC) == normalize (r,shiftCol cOff maxC)) [minR..maxR]) [0..] of
                                                          Nothing -> error "no loop"
                                                          Just c -> shiftCol (c-1) maxC
                                          rForTilable = case find (\rOff -> all (\c -> normalize (minR, c) == normalize (shiftRow rOff maxR,c)) [minC..cForTilable]) [0..] of
                                                          Nothing -> error "no r loop"
                                                          Just r -> shiftRow (r-1) maxR
                                       in Map.fromList [ ((r, c), v)
                                                       | r <- [minR..Row $ unRow rForTilable]
                                                       , c <- [minC..Col $ unCol cForTilable]
                                                       , p <- maybeToList $ unrotate (r, c)
                                                       , v <- maybeToList $ normalize p `Map.lookup` gardens]

calculateAccessible height width start gardens n
  = let gardensR = fillOutRotatedS gardens height width
        (_,gardensRpointsOdd,gardensRpointsEven) = findFixedPointR (rotate (rStart,cStart)) gardensR
        ((minR,maxR),(minC,maxC)) = getBounds $ Set.toList $ gardensR
        (rStart,cStart) = rotate start
        sizeC = unCol maxC - unCol minC + 1
        sizeR = unRow maxR - unRow minR + 1

        fullTilesToLeft = (n - (unCol cStart - unCol minC) - 1) `div` sizeC
        fullTilesToRight = (n - (unCol maxC - unCol cStart)) `div` sizeC
        fullTilesToTop = (n - (unRow rStart - unRow minR) - 1) `div` sizeR
        fullTilesToBottom = (n - (unRow maxR - unRow rStart)) `div` sizeR
        gardenRpoints = fromIntegral (if n `mod` 2 == 0 then gardensRpointsEven else gardensRpointsOdd)

        fullTilesInMiddle = (fullTilesToTop + 1 + fullTilesToBottom) * (fullTilesToRight + 1 + fullTilesToBottom)
        distanceIntoLeftmostColumn = (n - (unCol cStart - unCol minC) - 1) `mod` sizeC
        distanceIntoRightmostColumn = (n - (unCol minC - unCol cStart) + 1) `mod` sizeC
        distanceIntoTopRow = (n - (unRow rStart - unRow minR) - 1) `mod` sizeR
        distanceIntoBottomRow = (n - (unRow minR - unRow rStart) + 1) `mod` sizeR
        horizMidSections = 1 + max (fullTilesToTop - 1) 0 + max (fullTilesToBottom - 1) 0
        vertMidSections = 1 + max (fullTilesToTop - 1) 0 + max (fullTilesToBottom - 1) 0

        edgeStep n = fromIntegral . Set.size . edgeStepG n
        edgeStepG n accessible = Set.filter (\(r,c) -> r >= minR && r <= maxR && c >= minC && c <= maxC)
                                            (iterate (megaStepRN normalizeR normalizeC gardensR)
                                                     (Set.filter (\(r,c) -> (normalizeR r, normalizeC c) `Set.member` gardensR)
                                                                 accessible)
                                             !! fromIntegral n)
        normalizeR r = Row $ unRow minR + ((unRow r - unRow minR) `mod` sizeR)
        normalizeC c = Col $ unCol minC + ((unCol c - unCol minC) `mod` sizeC)
        numAlongTopRow          = horizMidSections * edgeStep distanceIntoTopRow          (Set.fromList [(shiftRow 1 maxR, c)|c <- [shiftCol (-sizeC) minC..shiftCol sizeC maxC]])
        numAlongBottomRow       = horizMidSections * edgeStep distanceIntoBottomRow       (Set.fromList [(shiftRow (-1) minR, c)|c <- [shiftCol (-sizeC) minC..shiftCol sizeC maxC]])
        numAlongLeftmostColumn  = vertMidSections  * edgeStep distanceIntoLeftmostColumn  (Set.fromList [(r, shiftCol (-1) minC)| r <- [shiftRow (-sizeR) minR..shiftRow sizeR maxR]])
        numAlongRightmostColumn = vertMidSections  * edgeStep distanceIntoRightmostColumn (Set.fromList [(r, shiftCol 1 maxC)| r <- [shiftRow (-sizeR) minR..shiftRow sizeR maxR]])
        --- TL          TR
        ---   XXXXXXXXXX
        ---   X        X
        ---   X        X
        ---   XXXXXXXXXX
        --- BL          BR
        topLeftCorner      = edgeStepG distanceIntoTopRow    (Set.fromList $ [(shiftRow 1 maxR, c)   | c <- [shiftCol 1        maxC..shiftCol sizeC maxC]] ++
                                                                             [(r,shiftCol 1 maxC)    | r <- [shiftRow 1        maxR..shiftRow sizeR maxR]])
        topRightCorner     = edgeStepG distanceIntoTopRow    (Set.fromList $ [(shiftRow 1 maxR, c)   | c <- [shiftCol (-sizeC) minC..shiftCol (-1)  minC]] ++
                                                                             [(r,shiftCol (-1) minC) | r <- [shiftRow 1        maxR..shiftRow sizeR maxR]])
        bottomRightCorner  = edgeStepG distanceIntoBottomRow (Set.fromList $ [(shiftRow (-1) minR, c)| c <- [shiftCol (-sizeC) minC..shiftCol (-1)  minC]] ++
                                                                             [(r,shiftCol (-1) minC) | r <- [shiftRow (-sizeR) minR..shiftRow (-1)  minR]])
        bottomLeftCorner   = edgeStepG distanceIntoBottomRow (Set.fromList $ [(shiftRow (-1) minR, c)| c <- [shiftCol 1        maxC..shiftCol sizeC maxC]] ++
                                                                             [(r,shiftCol 1 maxC)    | r <- [shiftRow (-sizeR) minR..shiftRow (-1)  minR]])
        numInTopLeftCorner     = fromIntegral . Set.size $ topLeftCorner
        numInTopRightCorner    = fromIntegral . Set.size $ topRightCorner
        numInBottomRightCorner = fromIntegral . Set.size $ bottomRightCorner
        numInBottomLeftCorner  = fromIntegral . Set.size $ bottomLeftCorner
     in trace (show ((rStart,cStart),
                     ((minR,maxR),(minC,maxC)),
                     (sizeC,sizeR),
                     ("full tile counts LRTB",(fullTilesToLeft, fullTilesToRight, fullTilesToTop, fullTilesToBottom), fullTilesInMiddle),
                     gardenRpoints,
                     ("distance into last tile LRTB",distanceIntoLeftmostColumn, distanceIntoRightmostColumn, distanceIntoTopRow, distanceIntoBottomRow),
                     ("num along last tiles TBLR", numAlongTopRow, numAlongBottomRow, numAlongLeftmostColumn, numAlongRightmostColumn),
                     ("num in corners TL,TR,BL,BR", numInTopLeftCorner, numInTopRightCorner, numInBottomLeftCorner, numInBottomRightCorner))
              )
              ((fullTilesInMiddle * gardenRpoints) + numAlongTopRow + numAlongBottomRow + numAlongLeftmostColumn + numAlongRightmostColumn + numInTopLeftCorner + numInTopRightCorner + numInBottomLeftCorner + numInBottomRightCorner)


gardenEverywhere n = Set.fromList [(Row r, Col c) | r <- [0..n],c<-[0..n]]

iterateAndPrintRotated iterations height width start garden
 = let gardenR = Set.map rotate $ garden
    in forM_ (take 25 $ iterate (stepR gardenR) (Set.singleton (rotate start)))
             (printAtPoints . (\accessible -> Map.fromList [(p,if p `Set.member` accessible then "O" else (if p `Set.member` gardenR then "." else "#"))
                                                           |r <- [0..unRow height],c<-[0..unCol width], let p = rotate (Row r, Col c)]))



part2Nope :: Parsed -> IO ()
part2Nope (height,width,start,gardens) =
  do print start
     print $ step gardens (Set.singleton start)
     forM_ (take 10 $ iterate (step (gardenEverywhere 40)) (Set.singleton (Row 20,Col 20))) (printGardens 0 0 40 40 (gardenEverywhere 40))
     forM_ (take 10 $ iterate (step gardens) (Set.singleton start)) (printGardens 0 0 (unRow height) (unCol width) gardens)
     forM_ (take 10 $ iterate (step (gardenEverywhere 5)) (Set.singleton (Row 3,Col 3))) (printGardens 0 0 5 5 (gardenEverywhere 5))
     forM_ (take 25 $ iterate (step (gardenEverywhere 25)) (Set.singleton (Row 13,Col 13))) (printGardens 0 0 25 25 (gardenEverywhere 25))
     iterateAndPrintRotated 25 (Row 25) (Col 25) (Row 13, Col 13) (gardenEverywhere 25)
     iterateAndPrintRotated 25 height width start gardens
     putStrLn "==="
     printAtPoints $ Map.fromList [((r,c), show $ unRow r * 3 + unCol c) | (r,c) <- [(Row 1, Col 0),(Row 1, Col 1),(Row 1, Col 2)]]
     printAtPoints $ fillOutRotated (Map.fromList [((r,c), show $ unRow r * 3 + unCol c) | (r,c) <- [(Row 1, Col 0),(Row 1, Col 1),(Row 1, Col 2)]]) (Row 3) (Col 3)
     putStrLn "==="
     printAtPoints $ fillOutRotated (Map.fromList [((r,c), show $ unRow r * 3 + unCol c) | (r,c) <- [(Row 0, Col 0),(Row 0, Col 1),(Row 0, Col 2)
                                                                                                    ,(Row 1, Col 0),(Row 1, Col 1),(Row 1, Col 2)
                                                                                                    ,(Row 2, Col 0),(Row 2, Col 1),(Row 2, Col 2)]]) (Row 3) (Col 3)
     putStrLn "==="
     printAtPoints $ fillOutRotated (Map.fromList [((r,c), show $ unRow r * 3 + unCol c) | (r,c) <- [(Row 0, Col 0),(Row 0, Col 1),(Row 0, Col 2)
                                                                                                    ,(Row 1, Col 0)               ,(Row 1, Col 2)
                                                                                                    ,(Row 2, Col 0),(Row 2, Col 1),(Row 2, Col 2)]]) (Row 3) (Col 3)
     print $ findFixedPoint start gardens
     print . Set.size . (!! 131) $ iterate (step gardens) (Set.singleton start)
     print . Set.size . (!! 140) $ iterate (step gardens) (Set.singleton start)
     print $ getBounds $ Set.toList $ fillOutRotatedS gardens height width
     printAtPointsR $ fillOutRotated (Map.fromList . fmap (,".") . Set.toList $ gardens) height width

     print $ findFixedPointR start $ fillOutRotatedS gardens height width
     forM_ [2,3,4,5,7,10,22,23,30] $ \n -> do
       putStrLn $ "trial " ++ show n
       putStrLn $ "  basic = " ++ show (Set.size . (!! n) $ iterate (megaStep height width gardens) (Set.singleton start))
       let s = calculateAccessible height width start gardens $ fromIntegral n
       putStrLn $ "  fancy = " ++ show s
     print 50
     print . Set.size . (!! 50) $ iterate (megaStep height width gardens) (Set.singleton start)
     print $ calculateAccessible height width start gardens 50
     print 100
     print $ calculateAccessible height width start gardens 100
     print 1000
     print $ calculateAccessible height width start gardens 1000
     putStrLn "heaps"
     print $ calculateAccessible height width start gardens 26501365

printGardens minRow minCol maxRow maxCol garden accessible = do putStrLn "========================="
                                                                forM_ [minRow..maxRow] $ \r -> do
                                                                  forM_ [minCol..maxCol] $ \c ->
                                                                    putStr $ if (Row r, Col c) `Set.member` accessible
                                                                               then "O"
                                                                               else if (Row r, Col c) `Set.member` garden then "." else "#"
                                                                  putStrLn ""


day21 part args = do let filename = case args of
                                      [] -> "inputs/day21"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
