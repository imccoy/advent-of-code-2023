module Main where

import Data.Maybe
import System.Environment (getArgs)
import System.Exit (die)

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
-- NEXT DAY IMPORT

import Part (getPart, Part (Part1, Part2))

getDay :: String -> Maybe (Part -> [String] -> IO ())
getDay n =
  case n of
    "1" -> Just day1
    "2" -> Just day2
    "3" -> Just day3
    "4" -> Just day4
    -- NEXT DAY CASE
    _ -> Nothing


main :: IO ()
main = do
  args <- getArgs
  case args of
    (day:part:rest) -> 
      do
        day' <- fromMaybe (die $ "Unknown day " ++ day) (pure <$> getDay day)
        part' <- fromMaybe (die $ "Unknown part " ++ part) (pure <$> getPart part)
        (day' :: Part -> [String] -> IO ()) part' rest
    _ -> putStrLn "usage: <cmd> <day> <part>"
