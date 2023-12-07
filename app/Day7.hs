{-# LANGUAGE FlexibleContexts #-}
module Day7 (day7, organised) where

import Part (Part (Part1, Part2))

import Control.Monad
import Data.List
import Text.Parsec

data Card = CardJoker
          | Card2 | Card3 | Card4 | Card5
          | Card6 | Card7 | Card8 | Card9
          | CardT | CardJ | CardQ | CardK | CardA
  deriving (Eq, Show, Ord)

type Parsed = [([Card], Int)]

parseCard :: (Stream s m Char) => ParsecT s u m Card
parseCard = (char 'A' >> pure CardA) <|>
            (char 'K' >> pure CardK) <|>
            (char 'Q' >> pure CardQ) <|>
            (char 'J' >> pure CardJ) <|>
            (char 'T' >> pure CardT) <|>
            (char '9' >> pure Card9) <|>
            (char '8' >> pure Card8) <|>
            (char '7' >> pure Card7) <|>
            (char '6' >> pure Card6) <|>
            (char '5' >> pure Card5) <|>
            (char '4' >> pure Card4) <|>
            (char '3' >> pure Card3) <|>
            (char '2' >> pure Card2)

parseGame = do many1 (do hand <- count 5 parseCard
                         space
                         bid <- read <$> manyTill digit newline
                         pure (hand, bid)
                     )

parseInput :: String -> Parsed
parseInput = either (error . show) id . runParser parseGame () "none"

data Strength = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Show, Ord)

organised o = reverse . sort $ [(length g, g) | g <- group $ sort o]

strength cards = go (organised cards)
  where go ((5,_):_) = FiveOfAKind
        go ((4,_):_) = FourOfAKind
        go ((3,_):(2,_):_) = FullHouse
        go ((3,_):_) = ThreeOfAKind
        go ((2,_):(2,_):_) = TwoPair
        go ((2,_):_) = OnePair
        go _ = HighCard

jokeriseCard CardJ = CardJoker
jokeriseCard c = c

part1 :: Parsed -> IO ()
part1 game = do print game
                let bids = [bid
                           |(_,_,bid) <- sort [ (strength hand, hand, bid)
                                              | (hand, bid) <- game]
                           ]
                print [ (strength hand, hand, bid)
                      | (hand, bid) <- game
                      ]
                print . sum $ zipWith (*) [1..] bids

strength2 cards = let (jokers, nonJokers) = partition (== CardJoker) cards
                   in jokerUpgrade (length jokers) (strength nonJokers)

jokerUpgrade :: Int -> Strength -> Strength
jokerUpgrade 0 s = s
jokerUpgrade n s = jokerUpgrade (n-1) $ case s of
                                          FourOfAKind -> FiveOfAKind
                                          FullHouse -> FourOfAKind
                                          ThreeOfAKind -> FourOfAKind
                                          TwoPair -> FullHouse
                                          OnePair -> ThreeOfAKind
                                          HighCard -> OnePair
                                          _ -> s


part2 :: Parsed -> IO ()
part2 game = do let jokerisedGame = [(jokeriseCard <$> hand, bid) | (hand, bid) <- game]
                let bids = [bid
                           |(_,_,bid) <- sort [ (strength2 hand, hand, bid)
                                              | (hand, bid) <- jokerisedGame]
                           ]
                print [ (strength2 hand, hand, bid)
                      | (hand, bid) <- jokerisedGame
                      ]
                print . sum $ zipWith (*) [1..] bids



day7 part args = do let filename = case args of
                                     [] -> "inputs/day7"
                                     [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
