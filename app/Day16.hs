{-# LANGUAGE BangPatterns #-}
module Day16 (day16) where

import Control.Monad (forM_)
import Data.Bits (Bits ((.|.), shiftL, shiftR, testBit, clearBit))
import Data.List.Split (chunksOf)
import Data.Word (Word8, Word16)
import Numeric (readHex, showIntAtBase)

import Part (Part (Part1, Part2))

data Bit = B0 | B1
  deriving (Show)

parseInput :: String -> [Bit]
parseInput [] = []
parseInput (x:xs) = go x ++ parseInput xs
  where go '0' = [B0,B0,B0,B0]
        go '1' = [B0,B0,B0,B1]
        go '2' = [B0,B0,B1,B0]
        go '3' = [B0,B0,B1,B1]
        go '4' = [B0,B1,B0,B0]
        go '5' = [B0,B1,B0,B1]
        go '6' = [B0,B1,B1,B0]
        go '7' = [B0,B1,B1,B1]
        go '8' = [B1,B0,B0,B0]
        go '9' = [B1,B0,B0,B1]
        go 'A' = [B1,B0,B1,B0]
        go 'B' = [B1,B0,B1,B1]
        go 'C' = [B1,B1,B0,B0]
        go 'D' = [B1,B1,B0,B1]
        go 'E' = [B1,B1,B1,B0]
        go 'F' = [B1,B1,B1,B1]

data PacketData = Literal !Integer | Operator !Operation [Packet]
  deriving (Show)

data Packet = Packet !Int !Int !PacketData
  deriving (Show)

-- bytePos = 6
-- n = 5
-- 01234567|01234567
-- xxxxxx67|012xxxxx
-- 67012xxx
-- xxx67012
readBits :: Int -> [Bit] -> ([Bit], [Bit])
readBits = splitAt

readInteger :: [Bit] -> (Integer, [Bit])
readInteger bytes = let (intBits, bytes') = go bytes
                     in (bitsToIntegral intBits, bytes')
  where go bytes = let ((continue:bits), bytes') = readBits 5 bytes
                    in case continue of
                         B0 -> (bits, bytes')
                         B1 -> let (bits', bytes'') = go bytes'
                                in (bits ++ bits', bytes'')

readOperator :: [Bit] -> ([Packet], [Bit])
readOperator (B0:bytes) = let (num, bytes') = readBits 15 bytes
                           in readPacketsByLength (fromIntegral $ bitsToIntegral num) bytes'
readOperator (B1:bytes) = let (num, bytes') = readBits 11 bytes
                           in readPacketsByCount (fromIntegral $ bitsToIntegral num) bytes'

readPacketsByCount :: Int -> [Bit] -> ([Packet], [Bit])
readPacketsByCount 0 bytes = ([], bytes)
readPacketsByCount count bytes = let (packet, bytes') = readPacket bytes
                                     (packets, bytes'') = readPacketsByCount (count - 1) bytes'
                                  in (packet:packets, bytes'')


readAll [] = []
readAll bytes = let (packet, bytes') = readPacket bytes
                 in packet:(readAll bytes')

readPacketsByLength lengthInBits bytes = let (packetBytes, restBytes) = splitAt lengthInBits bytes
                                          in (readAll packetBytes, restBytes)

bitsToIntegral :: [Bit] -> Integer
bitsToIntegral = go 0
  where
    go !n [] = n
    go !n (B0:bs) = go (n * 2) bs
    go !n (B1:bs) = go (n * 2 + 1) bs

readPacket :: [Bit] -> (Packet, [Bit])
readPacket [] = error "No packet"
readPacket bytes = let (versionBits, bytes') = readBits 3 bytes
                       (typeIdBits, bytes'') = readBits 3 bytes'
                       typeId = fromIntegral . bitsToIntegral $ typeIdBits
                       (packetData, bytes''') = if typeId == 4
                                                  then let (n, bytes''') = readInteger bytes''
                                                        in (Literal n, bytes''')
                                                  else let (ps, bytes''') = readOperator bytes''
                                                        in (Operator (operationFromInt typeId) ps, bytes''')
                    in (Packet (fromIntegral $ bitsToIntegral versionBits) typeId packetData, bytes''')
                          


sumVersions :: Packet -> Int
sumVersions (Packet v _ op) = v + opVersions op
  where opVersions (Literal _) = 0
        opVersions (Operator _ ps) = sum . map sumVersions $ ps

part1 :: [Bit] -> IO ()
part1 input = do putStrLn . show . readPacket $ parseInput "D2FE28"
                 putStrLn . show . readPacket $ parseInput "38006F45291200"
                 putStrLn . show . sumVersions . fst . readPacket $ input

data Operation = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo
  deriving (Show)

operationFromInt 0 = Sum
operationFromInt 1 = Product
operationFromInt 2 = Minimum
operationFromInt 3 = Maximum
operationFromInt 5 = GreaterThan
operationFromInt 6 = LessThan
operationFromInt 7 = EqualTo

eval (Packet _ _ (Literal n)) = n
eval (Packet _ _ (Operator op packets)) = evalOp op packets

evalAll f packets = f $ eval <$> packets

evalBin f = evalAll (\(a:b:_) -> if f a b then 1 else 0)

evalOp Sum = evalAll sum
evalOp Product = evalAll product
evalOp Minimum = evalAll minimum
evalOp Maximum = evalAll maximum
evalOp GreaterThan = evalBin (>)
evalOp LessThan = evalBin (<)
evalOp EqualTo = evalBin (==)

part2 :: [Bit] -> IO ()
part2 = putStrLn . show . eval . fst . readPacket

day16 part args = do let filename = case args of
                                      [] -> "inputs/day16"
                                      [f] -> f
                     inputs <- parseInput <$> readFile filename
                     case part of
                       Part1 -> part1 inputs
                       Part2 -> part2 inputs
