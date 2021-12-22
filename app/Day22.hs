{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TemplateHaskell, RankNTypes #-}
module Day22 where

import Control.Lens
import Control.Lens.TH
import Data.List (intersect, (\\))
import Text.Parsec

import Part (Part (Part1, Part2))

data Operation = TurnOn | TurnOff
  deriving (Show, Eq)

data Range = Range { _rangeMin :: Int, _rangeMax :: Int }
  deriving (Show, Eq)
makeLenses ''Range

data Box = Box { _boxX :: Range, _boxY :: Range, _boxZ :: Range }
  deriving (Show, Eq)
makeLenses ''Box

parseOperation = try parseOn <|> parseOff
  where parseOn = do string "on"
                     pure TurnOn
        parseOff = do string "off"
                      pure TurnOff

parseRange = do start <- parseNumber
                string ".."
                end <- parseNumber
                pure $ Range start end

parseNumber = do minus <- option "" (char '-' >>= pure . pure)
                 read . (minus ++) <$> many1 digit

parseLine = do operation <- parseOperation
               string " x="
               xrange <- parseRange
               string ",y="
               yrange <- parseRange
               string ",z="
               zrange <- parseRange
               pure (operation, Box xrange yrange zrange)

parseInput :: String -> [(Operation,Box)]
parseInput = fmap (either (error . show) id . runParser parseLine () "none") . lines

rangeSize :: Range -> Integer
rangeSize r = fromIntegral $ (r ^. rangeMax) + 1 - (r ^. rangeMin)

boxStrings (a,common,b) = "left:\n" ++ (unlines . fmap boxString $ a) ++ "\n\ncommon:\n" ++ (unlines . fmap boxString $ common) ++ "\n\nright:\n" ++ (unlines . fmap boxString $ b)

boxString :: Box -> String
boxString (Box x y z) = "x=" ++ rangeString x ++ ",y=" ++ rangeString y ++ ",z=" ++ rangeString z
  where rangeString (Range min max) = show min ++ ".." ++ show max

smashVolumes vol1 vol2 = findCommon (applySplitters vol2 [vol1],applySplitters vol1 [vol2])
  where applySplitters :: Box -> [Box] -> [Box]
        applySplitters splitter = splitVolumesMinBy boxX splitter . splitVolumesMaxBy boxX splitter . splitVolumesMinBy boxZ splitter . splitVolumesMaxBy boxZ splitter . splitVolumesMinBy boxY splitter . splitVolumesMaxBy boxY splitter
        splitVolumesMinBy :: Lens' Box Range -> Box -> [Box] -> [Box]
        splitVolumesMinBy axis splitter volumes = concat [ if vol ^. axis . rangeMin < splitPoint && splitPoint <= vol ^. axis . rangeMax
                                                             then [axis . rangeMax .~ (splitPoint - 1) $ vol
                                                                  ,axis . rangeMin .~ splitPoint $ vol]
                                                             else [vol]
                                                         | vol <- volumes]
          where splitPoint = splitter ^. axis . rangeMin
        splitVolumesMaxBy :: Lens' Box Range -> Box -> [Box] -> [Box]
        splitVolumesMaxBy axis splitter volumes = concat [ if vol ^. axis . rangeMin <= splitPoint && splitPoint < vol ^. axis . rangeMax
                                                             then [axis . rangeMin .~ (splitPoint + 1) $ vol
                                                                  ,axis . rangeMax .~ splitPoint $ vol]
                                                             else [vol]
                                                         | vol <- volumes]
          where splitPoint = splitter ^. axis . rangeMax

        findCommon (vols1, vols2) = let common = intersect vols1 vols2
                                     in (vols1 \\ common, common, vols2 \\ common)

turnOn newBox (existing:existingBoxes) = let (newSplit, common, existingSplit) = smashVolumes newBox existing
                                          in case (common,existingSplit) of
                                               (_,[]) -> turnOn newBox existingBoxes
                                               ([],_) -> existing:(turnOn newBox existingBoxes)
                                               _ -> existing:(foldr (\newBox existings -> turnOn newBox existings) existingBoxes newSplit)
turnOn newBox [] = [newBox]

turnOff newBox (existing:existingBoxes) = let (_, common, remaining) = smashVolumes newBox existing
                                           in (case common of
                                                 [] -> [existing]
                                                 _ -> remaining) ++ turnOff newBox existingBoxes
turnOff newBox [] = []


applyOperation :: (Operation, Box) -> [Box] -> [Box]
applyOperation (TurnOn, newBox) existingBoxes = turnOn newBox existingBoxes
applyOperation (TurnOff, newBox) existingBoxes = turnOff newBox existingBoxes

applyOperations = foldl (flip applyOperation) []

countCubes :: [Box] -> Integer
countCubes = sum . fmap (\(Box xs ys zs) -> rangeSize xs * rangeSize ys * rangeSize zs)

part1 :: [(Operation,Box)] -> IO ()
part1 = putStrLn . show . countCubes . applyOperations . filter (\(op, box) -> all (\n -> -50 <= n && n <= 50)
                                                                                   [box ^. boxX . rangeMin
                                                                                   ,box ^. boxX . rangeMax
                                                                                   ,box ^. boxY . rangeMin
                                                                                   ,box ^. boxY . rangeMax
                                                                                   ,box ^. boxZ . rangeMin
                                                                                   ,box ^. boxZ . rangeMax
                                                                                   ]
                                                                )

part2 :: [(Operation,Box)] -> IO ()
part2 = putStrLn . show . countCubes . applyOperations

day22 part args = do let filename = case args of
                                      [] -> "inputs/day22"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
