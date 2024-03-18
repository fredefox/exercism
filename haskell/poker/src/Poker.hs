{-# language GHC2021 #-}
{-# language DerivingStrategies #-}
module Poker (bestHands) where

import qualified Data.List as List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Ord

data Card = Card Char Char
deriving stock instance Eq Card

newtype Hand = Hand (Vector Card)

instance Eq Hand where
  h0 == h1 = case h0 `compare` h1 of
    EQ -> True
    _ -> False

instance Ord Hand where
  compare = comparing valuate

data Value
  = StraightFlush Card
  | Flush Card
  | Straight Card
  | FullHouse Card Card
  | Three Card
  | Two Card
  | High Card

valuate :: Hand -> Value
valuate (Hand h) = undefined

parseCard :: String -> Card
parseCard (x:y:_) = Card x y
parseCard _ = undefined

parseHand :: String -> Hand
parseHand = Hand . Vector.fromList . fmap parseCard . words

showCard :: Card -> String
showCard (Card a b) = [a, b]

showHand :: Hand -> String
showHand (Hand v) = unwords $ fmap showCard $ Vector.toList v

bestHands :: [String] -> Maybe [String]
bestHands = pure . fmap showHand . maximums . fmap parseHand

maximums :: Ord a => [a] -> [a]
maximums xs = case List.sort xs of
  [] -> []
  ys@(x:_) -> takeWhile (== x) ys
