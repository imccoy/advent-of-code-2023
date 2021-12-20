{-# LANGUAGE BlockArguments #-}

module Day19 where

import Control.Monad (forM, void)
import Data.List (intersect, nub, permutations, union)
import Debug.Trace (trace)
import Text.Parsec

import Part (Part (Part1, Part2))

data ScannerInput = ScannerInput Int [(Int, Int,Int)]
  deriving (Show, Eq)

parseScannerInputs = many parseScannerInput

parseScannerInput = do string "--- scanner "
                       scannerNumber <- read <$> many1 digit
                       string " ---\n"
                       points <- parsePoints
                       pure $ ScannerInput scannerNumber points

parsePoints = manyTill parsePoint (eof <|> (void $ try $ char '\n'))

parsePoint = do x <- parseCoord
                char ','
                y <- parseCoord
                char ','
                z <- parseCoord
                char '\n'
                pure (x,y,z)

parseCoord = do minus <- option "" (char '-' >>= pure . pure)
                read . (minus ++) <$> many1 digit

parseInput :: String -> [ScannerInput]
parseInput = either (error . show) id . runParser parseScannerInputs () "none"

data Axis = X | Y | Z
  deriving (Show, Eq)
data Flip = Flip | Straight
  deriving (Show, Eq)
data Orientation = Orientation Axis Flip Axis Flip Axis Flip
  deriving (Show, Eq)

data Cloud = Cloud [(Int, Orientation)] [(Int,Int,Int)]
  deriving (Show, Eq)

applyOrientation (Orientation xa xf ya yf za zf) (x, y, z) = (get xa xf, get ya yf, get za zf)
  where fetch X = x
        fetch Y = y
        fetch Z = z
        applyFlip Flip n = 0 - n
        applyFlip Straight n = n
        get a f = applyFlip f $ fetch a

allAxesOrientations :: [Orientation]
allAxesOrientations = do axes <- permutations [X,Y,Z]
                         [(x, xf),(y,yf),(z,zf)] <- forM axes $ \axis -> do
                                                       flip <- [Flip, Straight]
                                                       pure (axis, flip)
                         pure $ Orientation x xf y yf z zf

cloud0 (ScannerInput n points) = Cloud [(n, Orientation X Straight Y Straight Z Straight)] points

cloudsFrom :: ScannerInput -> [Cloud]
cloudsFrom (ScannerInput n points) = do orientation <- allAxesOrientations
                                        pure $ Cloud [(n, orientation)] $ applyOrientation orientation <$> points

applyOffset (xd, yd, zd) (x, y, z) = (x + xd, y + yd, z + zd)

matchesFor ps1 ps2 offset = intersect ps1 (applyOffset offset <$> ps2)

atLeastNElements 0 _ = True
atLeastNElements n [] = False
atLeastNElements n (_:xs) = atLeastNElements (n - 1) xs

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] ys = []
allPairs (x:xs) ys = go x (allPairs xs ys) ys
  where go _ pairs [] = pairs
        go x pairs (y:ys) = go x ((x,y):pairs) ys

linkClouds (Cloud [] []) x = [x]
linkClouds (Cloud meta1 ps1) (Cloud meta2 ps2) = do let offsets = nub $ do ((x1, y1, z1),(x2,y2,z2)) <- allPairs ps1 ps2
                                                                           pure (x1 - x2, y1 - y2, z1 - z2)
                                                    viableMatch <- filter ((atLeastNElements 12) . matchesFor ps1 ps2) offsets
                                                    let newPoints = union ps1 (applyOffset viableMatch <$> ps2)
                                                    trace (show (length meta1 + length meta2) ++ ": " ++ show (length newPoints) ++ ", viable match for " ++ show meta1 ++ " and " ++ show meta2 ++ " at " ++ show viableMatch) $ pure $ Cloud (meta1 ++ meta2) newPoints

filterInputIds pred = filter (\(ScannerInput n _) -> pred n)
allUnlinked (Cloud meta _) scannerInputs = let cloudsIds = map fst meta
                                            in filterInputIds (\n -> elem n cloudsIds) scannerInputs
cloudIds (Cloud meta _) = fst <$> meta

searchAll :: [ScannerInput] -> [Cloud]
searchAll allInputs = go  (Cloud [] []) $ elemsRests allInputs
  where go :: Cloud -> [(ScannerInput, [ScannerInput])] -> [Cloud]
        go cloud ((input,inputs):rest)
          = case concat . fmap (linkClouds cloud) $ cloudsFrom input of
              [] -> go cloud rest
              clouds -> do cloud <- clouds
                           go cloud . elemsRests $ filterInputIds (\n -> not . elem n $ cloudIds cloud) inputs
        go cloud [] = [cloud]

elemsRests :: [a] -> [(a,[a])]
elemsRests xs = go [] xs
  where go _ [] = []
        go earlier (x:xs) = (x,xs ++ earlier):(go (x:earlier) xs)
                                      

part1 :: [ScannerInput] -> IO ()
part1 inputs = case searchAll $ inputs of
                 ((Cloud _ ps):_) -> putStrLn $ "DONE!!!!!" ++ show (length ps)

part2 :: [ScannerInput] -> IO ()
part2 _ = 
  let relativePositions = [ (0, 0, 0)
                          , (-29,-1057,-105)
                          , (-86,127,1153)
                          , (1085,-1120,-47)
                          , (-88,81,-1292)
                          , (-118,-2270,-157)
                          , (1117,-2390,-133)
                          , (-14,113,2259)
                          , (-1325,-1205,-103)
                          , (-18,1198,-75)
                          , (-111,2492,-12)
                          , (-109,2416,-1228)
                          , (-78,-1134,-1190)
                          , (-1191,-6,-1319)
                          , (-1149,-1171,-1360)
                          , (-1191,-2329,-1375)
                          , (-53,117,3543)
                          , (1140,135,-1377)
                          , (1080,2479,-57)
                          , (2325,2564,-142)
                          , (-129,2430,1044)
                          , (2447,99,-1322)
                          , (31,2493,2288)
                          , (-1216,-1120,-2416)
                          , (-45,-2280,1019)
                          , (3,3739,1197)
                          , (1150,2511,2339)
                          , (1152,1337,-1293)
                          , (-127,2412,3436)
                          , (-18,4915,1074)
                          , (-2409,-2252,-1199)
                          , (2303,3720,-146)
                          , (-1160,-2231,1123)
                          ]
  in putStrLn . show . maximum . allDistances $ relativePositions

allDistances :: [(Int,Int,Int)] -> [Int]
allDistances a = fmap manhattanDistance . allPairs a $ a

manhattanDistance ((x1,y1,z1),(x2,y2,z2)) = distance x1 x2 + distance y1 y2 + distance z1 z2
  where distance a b | a < b     = b - a
                     | otherwise = a - b

day19 part args = do let filename = case args of
                                      [] -> "inputs/day19"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
