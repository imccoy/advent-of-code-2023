module Main where

import Data.Maybe
import System.Environment (getArgs)
import System.Exit (die)

import Lib
import Day1 (day1)
import Day2 (day2)
import Part (getPart, Part (Part1, Part2))

getDay :: String -> Maybe (Part -> [String] -> IO ())
getDay n =
  case n of
    "1" -> Just day1
    "2" -> Just day2
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
