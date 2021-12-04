module Day3.Common where

import Data.Foldable

data Bit = Bit0 | Bit1
  deriving (Eq)

bitToInt Bit0 = 0
bitToInt Bit1 = 1

invertBit Bit0 = Bit1
invertBit Bit1 = Bit0

data CountPair = CountPair { zeros :: !Int, ones :: !Int }

instance (Semigroup CountPair) where
  a <> b = CountPair (zeros a + zeros b) (ones a + ones b)

instance (Monoid CountPair) where
  mempty = CountPair 0 0

bitToCountPair Bit0 = CountPair 1 0
bitToCountPair Bit1 = CountPair 0 1

charToBit '0' = Bit0
charToBit '1' = Bit1
charToBit s = error $ "Invalid charToBit digit" ++ [s]

mostCommonBit :: CountPair -> Bit
mostCommonBit pair
  | zeros pair > ones pair = Bit0
  | otherwise              = Bit1

leastCommonBit :: CountPair -> Bit
leastCommonBit pair
  | zeros pair <= ones pair = Bit0
  | otherwise               = Bit1

toInt :: (Foldable f) => f Bit -> Int
toInt = foldl' (\n b -> 2 * n + bitToInt b) 0

