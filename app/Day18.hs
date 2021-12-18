{-# LANGUAGE LambdaCase #-}
module Day18 (day18) where

import Control.Monad (void, forM_, (<=<))
import Control.Monad.ST
import Data.Either (either)
import Data.Foldable (foldl')
import Data.STRef
import Text.Parsec

import Part (Part (Part1, Part2))

data SNNumber = SNSingle !Int | SNPair !SNNumber !SNNumber
  deriving (Show)

magnitude (SNSingle n) = n
magnitude (SNPair l r) = 3 * magnitude l + 2 * magnitude r

parseSNNumber = parsePair <|> parseSingle
  where parsePair = do void $ char '['
                       left <- parseSNNumber
                       void $ char ','
                       right <- parseSNNumber
                       void $ char ']'
                       pure $ SNPair left right
        parseSingle = do digits <- many1 digit
                         pure $ SNSingle (read digits)

parseInput :: String -> [SNNumber]
parseInput = fmap parseLine . lines
parseLine = either (error . show) id . runParser parseSNNumber () "none"

newtype AddToLeft = AddToLeft Int
newtype AddToRight = AddToRight Int
data ExplodeResult
   = ExplodedLR !SNNumber AddToLeft AddToRight
   | ExplodedL !SNNumber AddToLeft
   | ExplodedR !SNNumber AddToRight
   | Exploded !SNNumber
   | Keep

explode = toDone . go 0
  where
    go 4 (SNPair (SNSingle left) (SNSingle right))
      = ExplodedLR (SNSingle 0) (AddToLeft left) (AddToRight right)
    go 4 (SNSingle _) = Keep
    go 4 v = error $ "too deep, no match: " ++ snString v
    go depth (SNSingle n) = Keep
    go depth (SNPair left right) = case go (depth + 1) left of
                                     Exploded t -> Exploded (SNPair t right)
                                     ExplodedLR t addToLeft (AddToRight n) -> ExplodedL (SNPair t (addAtLeftmost n right)) addToLeft
                                     ExplodedR t (AddToRight n) -> Exploded (SNPair t (addAtLeftmost n right))
                                     ExplodedL t addToLeft -> ExplodedL (SNPair t right) addToLeft
                                     Keep -> case go (depth + 1) right of
                                               Exploded t -> Exploded (SNPair left t)
                                               ExplodedLR t (AddToLeft n) addToRight -> ExplodedR (SNPair (addAtRightmost n left) t) addToRight
                                               ExplodedL t (AddToLeft n) -> Exploded (SNPair (addAtRightmost n left) t)
                                               ExplodedR t addToRight -> ExplodedR (SNPair left t) addToRight
                                               Keep -> Keep
    addAtLeftmost n (SNSingle m) = SNSingle $ n + m
    addAtLeftmost n (SNPair l r) = SNPair (addAtLeftmost n l) r
    addAtRightmost n (SNSingle m) = SNSingle $ n + m
    addAtRightmost n (SNPair l r) = SNPair l (addAtRightmost n r)
    toDone (ExplodedLR n _ _) = Just n
    toDone (ExplodedL n _) = Just n
    toDone (ExplodedR n _) = Just n
    toDone (Exploded n) = Just n
    toDone (Keep) = Nothing

data SNLinked s = SNLSingle (Maybe (STRef s (SNLinked s))) Int (Maybe (STRef s (SNLinked s)))
                | SNLPair (STRef s (SNLinked s)) (STRef s (SNLinked s))

makeSNLinked :: SNNumber -> ST s (STRef s (SNLinked s))
makeSNLinked snNum = do initial <- buildInitial snNum
                        rightmost <- addLeftLinks Nothing initial
                        addRightLinks rightmost Nothing
                        pure initial
  where buildInitial :: SNNumber -> ST s (STRef s (SNLinked s))
        buildInitial (SNSingle n) = newSTRef $ SNLSingle Nothing n Nothing
        buildInitial (SNPair l r) = do l' <- buildInitial l
                                       r' <- buildInitial r
                                       newSTRef $ SNLPair l' r'
        addLeftLinks :: Maybe (STRef s (SNLinked s)) -> STRef s (SNLinked s) -> ST s (STRef s (SNLinked s))
        addLeftLinks leftRef nodeRef = do node <- readSTRef nodeRef
                                          case node of
                                            SNLSingle _ n _ -> do writeSTRef nodeRef (SNLSingle leftRef n Nothing)
                                                                  pure nodeRef
                                            SNLPair l r -> do lastLeftRef <- addLeftLinks leftRef l
                                                              addLeftLinks (Just lastLeftRef) r
        addRightLinks :: STRef s (SNLinked s) -> Maybe (STRef s (SNLinked s)) -> ST s ()
        addRightLinks curr prev = do (SNLSingle nextRef n rightRef) <- readSTRef curr
                                     writeSTRef curr (SNLSingle nextRef n prev)
                                     case nextRef of
                                       Nothing -> pure ()
                                       Just nextRef -> addRightLinks nextRef (Just curr)
 
 


unLinked :: STRef s (SNLinked s) -> ST s SNNumber
unLinked nodeRef = do
  readSTRef nodeRef >>= \case
    SNLSingle _ n _ -> pure $ SNSingle n
    SNLPair l r -> do l' <- unLinked l
                      r' <- unLinked r
                      pure $ SNPair l' r'

explode' sn = runST $ do
                linked <- makeSNLinked sn
                exploded <- go 0 linked
                case exploded of
                  True -> Just <$> unLinked linked
                  False -> pure Nothing
  where
    go :: Int -> STRef s (SNLinked s) -> ST s Bool
    go 4 nodeRef = readSTRef nodeRef >>= \case
                     (SNLSingle _ _ _) -> pure False
                     (SNLPair leftRef rightRef) -> do
                       (SNLSingle newPrev left _) <- readSTRef leftRef
                       (SNLSingle _ right newSucc) <- readSTRef rightRef
                       writeSTRef nodeRef $ SNLSingle newPrev 0 newSucc
                       adjustNode newPrev (+ left)
                       adjustNode newSucc (+ right)
                       pure True
    go depth nodeRef = readSTRef nodeRef >>= \case
                         (SNLSingle _ _ _) -> pure False
                         (SNLPair left right) -> go (depth + 1) left >>= \case
                           True -> pure $ True
                           False -> go (depth + 1) right
    adjustNode Nothing _ = pure ()
    adjustNode (Just ref) f = modifySTRef ref (\(SNLSingle prev n next) -> SNLSingle prev (f n) next)



split (SNSingle n) | n >= 10 = Just (SNPair (SNSingle (n `div` 2)) (SNSingle (n `div` 2 + n `mod` 2)))
                   | otherwise = Nothing
split (SNPair l r) = case split l of
                       Just l'-> Just (SNPair l' r)
                       Nothing -> case split r of
                                    Just r' -> Just (SNPair l r')
                                    Nothing -> Nothing

reduce n = case explode n of
             Just n' -> reduce n'
             Nothing -> case split n of
                          Just n'' -> reduce n''
                          Nothing -> n

addSN a b = reduce $ SNPair a b

snString (SNSingle n) = show n
snString (SNPair l r) = "[" ++ snString l ++ "," ++ snString r ++ "]"

part1 :: [SNNumber] -> IO ()
part1 inputs = do
  forM_ ["[[[[[9,8],1],2],3],4]","[7,[6,[5,[4,[3,2]]]]]","[[6,[5,[4,[3,2]]]],1]","[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]","[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"] $ \explodeTest -> do
    putStrLn $ explodeTest ++ " -> " ++ case explode . parseLine $ explodeTest of
                                          Just n -> snString n
                                          Nothing -> "NOTHING"
    putStrLn $ explodeTest ++ " -> " ++ case explode' . parseLine $ explodeTest of
                                          Just n -> snString n
                                          Nothing -> "NOTHING"
  putStrLn "==========="
  let sum = foldl' addSN (head inputs) $ tail inputs
  putStrLn $ snString sum
  putStrLn $ show . magnitude $ sum

part2 :: [SNNumber] -> IO ()
part2 inputs = do
  let m = maximum $ do a <- inputs
                       b <- inputs
                       pure $ magnitude $ addSN a b
  putStrLn $ show m

day18 part args = do let filename = case args of
                                      [] -> "inputs/day18"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
