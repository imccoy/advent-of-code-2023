{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TemplateHaskell, RankNTypes, BangPatterns #-}
module Day24 (day24) where

import Control.Monad (void, forM_)
import Data.Either (partitionEithers)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import System.Exit (exitFailure)
import Text.Parsec

import Part (Part (Part1, Part2))

data Var = W | X | Y | Z
  deriving (Show, Eq)

data Op = VarOp Var | LiteralOp Integer
  deriving (Show, Eq)

data Instruction = Inp Var
                 | Add Var Op
                 | Mul Var Op
                 | Div Var Op
                 | Mod Var Op
                 | Eql Var Op
  deriving (Show, Eq)

parseNumber = do minus <- option "" (char '-' >>= pure . pure)
                 read . (minus ++) <$> many1 digit

parseVar = try (char 'w' >>= (const $ pure W))
         <|> try (char 'x' >>= (const $ pure X))
         <|> try (char 'y' >>= (const $ pure Y))
         <|> try (char 'z' >>= (const $ pure Z))

parseOp = try (VarOp <$> parseVar) <|> (LiteralOp <$> parseNumber)

parseInp = do try (string "inp ")
              Inp <$> parseVar

parseBinop = parseBinop' "add" Add <|> parseBinop' "mul" Mul <|> parseBinop' "div" Div
           <|> parseBinop' "mod" Mod <|> parseBinop' "eql" Eql

parseBinop' :: String -> (Var -> Op -> Instruction) -> Parsec String y Instruction
parseBinop' instr constr = do try (string instr)
                              char ' '
                              op1 <- parseVar
                              char ' '
                              op2 <- parseOp
                              pure $ constr op1 op2


parseInstruction = parseInp <|> parseBinop

parseInstructions = endBy parseInstruction ((void (char '\n')) <|> eof)

parseInput :: String -> [Instruction]
parseInput input = either (error . show) id . runParser parseInstructions () "none" $ input

data Registers = Registers { regW :: !Integer
                           , regX :: !Integer
                           , regY :: !Integer
                           , regZ :: !Integer }

setReg W val registers = registers { regW = val }
setReg X val registers = registers { regX = val }
setReg Y val registers = registers { regY = val }
setReg Z val registers = registers { regZ = val }

getReg W registers = regW registers
getReg X registers = regX registers
getReg Y registers = regY registers
getReg Z registers = regZ registers

getVal (LiteralOp n) _ = n
getVal (VarOp var) registers = getReg var registers

runInstruction (!registers, (input:inputs)) (Inp reg) = (setReg reg input registers, inputs)
runInstruction state (Add op1 op2) = runBinOp state (+) op1 op2
runInstruction state (Mul op1 op2) = runBinOp state (*) op1 op2
runInstruction state (Div op1 op2) = runBinOp state divTrunc op1 op2
runInstruction state (Mod op1 op2) = runBinOp state mod op1 op2
runInstruction state (Eql op1 op2) = runBinOp state (\a b -> if a == b then 1 else 0) op1 op2

divTrunc a b = (abs a `div` abs b) * aneg * bneg
  where aneg = if a < 0 then -1 else 1
        bneg = if b < 0 then -1 else 1

runBinOp :: (Registers, [Integer]) -> (Integer -> Integer -> Integer) -> Var -> Op -> (Registers, [Integer])
runBinOp (registers, inputs) f op1 op2 = (setReg op1 (f (getReg op1 registers) (getVal op2 registers)) registers, inputs)

runMachine registers inputs instructions = foldl runInstruction (registers, inputs) instructions

loopCode (a,b,c) = (Inp W):
                   (Mul X (LiteralOp 0)):
                   (Add X (VarOp Z)):
                   (Mod X (LiteralOp 26)):
                   (Div Z (LiteralOp a)):
                   (Add X (LiteralOp b)):
                   (Eql X (VarOp W)):
                   (Eql X (LiteralOp 0)):
                   (Mul Y (LiteralOp 0)):
                   (Add Y (LiteralOp 25)):
                   (Mul Y (VarOp X)):
                   (Add Y (LiteralOp 1)):
                   (Mul Z (VarOp Y)):
                   (Mul Y (LiteralOp 0)):
                   (Add Y (VarOp W)):
                   (Add Y (LiteralOp c)):
                   (Mul Y (VarOp X)):
                   (Add Z (VarOp Y)):
                   []

findLoop :: [Instruction] -> Maybe ((Integer,Integer,Integer), [Instruction])
findLoop ((Inp W):
          (Mul X (LiteralOp 0)):
          (Add X (VarOp Z)):
          (Mod X (LiteralOp 26)):
          (Div Z (LiteralOp a)):
          (Add X (LiteralOp b)):
          (Eql X (VarOp W)):
          (Eql X (LiteralOp 0)):
          (Mul Y (LiteralOp 0)):
          (Add Y (LiteralOp 25)):
          (Mul Y (VarOp X)):
          (Add Y (LiteralOp 1)):
          (Mul Z (VarOp Y)):
          (Mul Y (LiteralOp 0)):
          (Add Y (VarOp W)):
          (Add Y (LiteralOp c)):
          (Mul Y (VarOp X)):
          (Add Z (VarOp Y)):
          rest) = Just ((a,b,c),rest)
findLoop (_:rest) = Nothing

findLoops :: Int -> [Instruction] -> IO [(Integer,Integer,Integer)]
findLoops _ [] = pure []
findLoops n input = do putStrLn $ "round " ++ show n ++ ":"
                       case findLoop input of
                         Just (vars, rest) -> do putStrLn $ show vars
                                                 (vars:) <$> findLoops (n+1) rest
                         Nothing -> do putStrLn $  "NO LOOP" ++ show input
                                       pure []

-- X = ((Z0 % 26 + b) == inpN) == 0
-- Z = Z0 / a * (25 * X + 1) + (inpN + c) * X
--
-- let Zs = Z digits base-26
-- X = Zs_least + b != inpN
-- Zs' = if a == 26 then Zs >> 1 else Zs
-- Zs = if X then (Zs' << 1) + (inpN + c) else Zs'
--

simsim :: [Integer] -> (Integer, Integer, Integer) -> Integer -> [Integer]
simsim zs (a,b,c) input = let zs' = if a == 26 then shiftR zs else zs
                           in if lsbZ zs + b == input
                                then zs'
                                else shiftL zs' `addZ` (input + c)
  where shiftL zs = 0:zs
        shiftR (_:zs) = zs
        shiftR [] = []
        lsbZ (z:_) = z
        lsbZ [] = 0
        addZ (z:zs) n | n + z >= 26 = error "adding too big!"
                      | otherwise   = (z+n):zs

simsim2 zs (a,b,c) input = let (z, zs') = if a == 26 then pop zs else peek zs
                            in if z + b == input
                              then zs'
                              else push (input+c) zs'
  where peek [] = (0, [])
        peek (z:zs) = (z, z:zs)
        pop [] = (0, [])
        pop (z:zs) = (z, zs)
        push z zs = z:zs

simIO zs (a,b,c) input = do putStrLn $ concat ["a=",show a,", b=",show b,", c=",show c,", input=",show input]
                            (z, zs') <- if a == 26
                                          then do putStrLn "Popping"
                                                  pure $ pop zs
                                          else do putStrLn "Peeking"
                                                  pure $ peek zs
                            putStrLn $ show z ++ " + " ++ show b ++ " == " ++ show input ++ "?"
                            if z + b == input
                              then do putStrLn $ "yes, no push"
                                      pure zs'
                              else do putStrLn $ "no, pushing " ++ show (input + c)
                                      pure $ push (input+c) zs'
  where peek [] = (0, [])
        peek (z:zs) = (z, z:zs)
        pop (z:zs) = (z, zs)
        push z zs = z:zs

explain [] _ zs = putStrLn $ "FINAL STACK " ++ show zs
explain (input:inputs) (params:paramss) zs = do zs' <- simIO zs params input
                                                putStrLn $ "STACK: " ++ show zs ++ "\n\n\n"
                                                explain inputs paramss zs'

toZs :: Integer -> [Integer]
toZs 0 = []
toZs n = (n `mod` 26):(toZs $ n `div` 26)

fromZs :: [Integer] -> Integer
fromZs [] = 0
fromZs (z:zs) = z + 26 * fromZs zs

simsimGo z iter input = fromZs $ simsim (toZs z) iter input
simsimGo2 z iter input = fromZs $ simsim2 (toZs z) iter input

ensureSimMatchesIter sim iter = do input <- [1..9]
                                   z0 <- [0..1000]
                                   let z = regZ (fst (runMachine (Registers 0 0 0 z0) [input] $ loopCode iter))
                                   let z' = sim z0 iter input
                                   if z == z' then [] else pure (iter, input, z0, z, z')

ensureSimMatches sim params =
  case concat (ensureSimMatchesIter sim <$> params) of
    [] -> putStrLn "simsim ok"
    mismatches -> do putStrLn "simsim misses"
                     mapM_ putStrLn . (fmap show) $  mismatches
                     exitFailure

matches True  True  = True
matches False False = True
matches _     _     = False

round26 n = (n `div` 26) * 26

testRule prefix iter ruleZ rule = do
  let trials = do input <- [1..9]
                  z0 <- [0..100]
                  let z = simsimGo z0 iter input
                  if ruleZ z 
                    then case rule input z0 of
                           Just z' -> pure $ if z' == z
                                               then Right $ "z0=" ++ show z0 ++ " -> " ++ "z=" ++ show z ++ " on " ++ show input
                                               else Left (input, z0, z, z')
                           Nothing -> []
                    else []
   in case partitionEithers trials of
        ([], hits) -> putStrLn $ prefix ++ " rule OK" ++ show hits
        (misses, _)  -> do putStrLn $ prefix ++ " rule bad: " ++ show misses
                           exitFailure

-- round 1 (1,11,5):    [input1 + 5]
-- round 2 (1,13,5):    [input2 + 5, input1 + 5]
-- round 3 (1,12,1):    [input3 + 1, input2 + 5, input1 + 5]
-- round 4 (1,15,15):   [input4 + 15, input3 + 1, input2 + 5, input1 + 5]
-- round 5 (1,10,2):    [input5 + 2, input4 + 15, input3 + 1, input2 + 5, input1 + 5]
-- round 6 (26,-1,2):   CONSTRAINT: input5 + 1 == input6
--                      [input4 + 15, input3 + 1, input2 + 5, input1 + 5]
-- round 7 (1,14,5):    [input7 + 5, input4 + 15, input3 + 1, input2 + 5, input1 + 5]
-- round 8 (26,-8,8):   CONSTRAINT: input7 - 3 == input8
--                      [input4 + 15, input3 + 1, input2 + 5, input1 + 5]
-- round 9 (26,-7,14):  CONSTRAINT: input4 + 8 == input9
--                      [input3 + 1, input2 + 5, input1 + 5]
-- round 10 (26,-8,12): CONSTRAINT: input3 - 7 = input10
--                      [input2 + 5, input1 + 5]
-- round 11 (1,11,7):   [input11 + 7, input2 + 5, input1 + 5]
-- round 12 (26,-2,14): CONSTRAINT: input11 + 5 == input12
--                      [input2 + 5, input1 + 5]
-- round 13 (26,-2,13): CONSTRAINT: input2 + 3 == input13
--                      [input1 + 5]
-- round 14 (26,-13,6): CONSTRAINT: input1 - 8 = input14
--                      []
--
-- 12345678901234
-- 96918996924991
--
-- 12345678901234
-- 91811241911641

--round12Rule iter = -- (a=26, b=-2, c=14
--  let ruleZ = \z -> z >= 0 && z <= 7
--      rule = (\input z0 -> if (z0 `mod` 26) + 2 == input then Just (round26 z0 + 14 + input) else Nothing)
--   in testRule "round 12" iter ruleZ rule

round13Rule iter = -- (a=26, b=-2, c=13)
  let ruleZ = \z -> True
      rule = (\input z0 -> if (z0 `mod` 26) - 2 == input
                             then if (z0 `div` 26) >= 14 && (z0 `div` 26) <= 22
                                    then Just (z0 `div` 26)
                                    else Nothing
                             else Just (round26 z0 + 13 + input)
             )
   in testRule "round 13" iter ruleZ rule


round14Rule iter =
  let ruleZ = \z -> z == 0
      rule = \input z0 -> if z0 - 13 == input then Just (z0 - 13 - input) else Nothing
   in testRule "round 14" iter ruleZ rule

part1 :: [Instruction] -> IO ()
part1 input = do params <- findLoops 1 input
                 ensureSimMatches simsimGo (reverse params)
                 ensureSimMatches simsimGo2 (reverse params)
                 explain [9,6,9,1,8,9,9,6,9,2,4,9,9,1] params []

part2 :: [Instruction] -> IO ()
part2 input = do params <- findLoops 1 input
                 explain [9,1,8,1,1,2,4,1,9,1,1,6,4,1] params []

day24 part args = do let filename = case args of
                                      [] -> "inputs/day24"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs






