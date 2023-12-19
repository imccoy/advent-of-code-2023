module Day19 (day19) where
import Part (Part (Part1, Part2))

import Control.Monad (void)
import Data.Maybe (fromMaybe, fromJust)
import Text.Parsec
import qualified Data.Map as Map

data Condition = ConditionGT Char Int | ConditionLT Char Int
  deriving (Show, Eq, Ord)

data Rule = ConditionRule Condition String | DirectRule String
  deriving (Show, Eq, Ord)

type Workflow = (String, [Rule])

type Parsed = ([Workflow],[(Int,Int,Int,Int)])

parseRule = try (do prop <- anyChar
                    op <- (char '<' >> pure ConditionLT) <|> (char '>' >> pure ConditionGT)
                    amount <- read <$> many1 digit
                    char ':'
                    dest <- many alphaNum
                    pure $ ConditionRule (op prop amount) dest
                ) <|> (DirectRule <$> many alphaNum)

parseWorkflow = do name <- many alphaNum
                   char '{'
                   rules <- sepBy parseRule (char ',')
                   char '}'
                   pure (name, rules)

parsePart = do char '{'
               string "x="
               x <- read <$> many1 digit
               char ','
               string "m="
               m <- read <$> many1 digit
               char ','
               string "a="
               a <- read <$> many1 digit
               char ','
               string "s="
               s <- read <$> many1 digit
               char '}'
               pure (x,m,a,s)

parseProblem = do workflows <- manyTill (do w <- parseWorkflow; newline; pure w) newline
                  parts <- manyTill (do p <- parsePart; newline; pure p) eof
                  pure (workflows, parts)


parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseProblem () "none"

isAccepted :: Map.Map String [Rule] -> (Int,Int,Int,Int) -> Bool
isAccepted workflows = go "in"
  where go "A" = const True
        go "R" = const False
        go workflowName = goR (fromJust $ Map.lookup workflowName workflows)
        goR [DirectRule d] piece = go d piece
        goR ((ConditionRule cond dest):conds) piece | meetsCond cond piece = go dest piece
                                                    | otherwise = goR conds piece
        meetsCond (ConditionGT prop val) piece = getProp prop piece > val
        meetsCond (ConditionLT prop val) piece = getProp prop piece < val
        getProp 'x' (x,_,_,_) = x
        getProp 'm' (_,m,_,_) = m
        getProp 'a' (_,_,a,_) = a
        getProp 's' (_,_,_,s) = s


part1 :: Parsed -> IO ()
part1 (workflows0, pieces) = let workflows = Map.fromList workflows0
                              in print . sum . map (\(x,m,a,s) -> x+m+a+s) . filter (isAccepted workflows) $ pieces


countConfigurations :: Map.Map String [Rule] -> ((Int,Int),(Int,Int),(Int,Int),(Int,Int)) -> Integer
countConfigurations workflows = go "in"
  where go "A" (xBounds,mBounds,aBounds,sBounds) = boundsSize xBounds * boundsSize mBounds * boundsSize aBounds * boundsSize sBounds
        go "R" _ = 0
        go workflowName bounds = goR (fromJust $ Map.lookup workflowName workflows) bounds
        boundsSize (min,max) | max < min = 0
                             | otherwise = fromIntegral $ max - min + 1
        goR [DirectRule d] bounds = go d bounds
        goR ((ConditionRule cond dest):conds) bounds = let (boundsIn, boundsOut) = refineBounds cond bounds
                                                        in go dest boundsIn + goR conds boundsOut
        refineBounds (ConditionGT prop val) bounds = refineCondition (refineGT val) prop bounds
        refineBounds (ConditionLT prop val) bounds = refineCondition (refineLT val) prop bounds
        refineCondition f 'x' (xBounds,mBounds,aBounds,sBounds) = let (i,o) = f xBounds
                                                                   in ((i,mBounds,aBounds,sBounds),(o,mBounds,aBounds,sBounds))
        refineCondition f 'm' (xBounds,mBounds,aBounds,sBounds) = let (i,o) = f mBounds
                                                                   in ((xBounds,i,aBounds,sBounds),(xBounds,o,aBounds,sBounds))
        refineCondition f 'a' (xBounds,mBounds,aBounds,sBounds) = let (i,o) = f aBounds
                                                                   in ((xBounds,mBounds,i,sBounds),(xBounds,mBounds,o,sBounds))
        refineCondition f 's' (xBounds,mBounds,aBounds,sBounds) = let (i,o) = f sBounds
                                                                   in ((xBounds,mBounds,aBounds,i),(xBounds,mBounds,aBounds,o))
        refineGT val (min,max) = ((val+1,max),(min,val))
        refineLT val (min,max) = ((min,val-1),(val,max))

part2 :: Parsed -> IO ()
part2 (workflows0, _) = let workflows = Map.fromList workflows0
                         in print $ countConfigurations workflows
                                                        ((1,4000)
                                                        ,(1,4000)
                                                        ,(1,4000)
                                                        ,(1,4000)
                                                        )

                     

day19 part args = do let filename = case args of
                                      [] -> "inputs/day19"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
