{-# language LambdaCase #-}
module TwelveDays (recite) where

import Text.Printf
import qualified Data.List as List

recite :: Int -> Int -> [String]
recite a b = fmap step $ enumFromTo a b

step :: Int -> String
step n = printf "On the %s day of Christmas my true love gave to me: %s in a Pear Tree." (numWord n) (thing n)

numWord :: Int -> String
numWord = (m !!) . pred
  where
  m =
    [ "first"
    , "second"
    , "third"
    , "fourth"
    , "fifth"
    , "sixth"
    , "seventh"
    , "eighth"
    , "ninth"
    , "tenth"
    , "eleventh"
    , "twelfth"
    ]

thing :: Int -> String
thing = mk . reverse . (`take` things)

things :: [String]
things =
  [ "a Partridge"
  , "two Turtle Doves"
  , "three French Hens"
  , "four Calling Birds"
  , "five Gold Rings"
  , "six Geese-a-Laying"
  , "seven Swans-a-Swimming"
  , "eight Maids-a-Milking"
  , "nine Ladies Dancing"
  , "ten Lords-a-Leaping"
  , "eleven Pipers Piping"
  , "twelve Drummers Drumming"
  ]

mk :: [String] -> String
mk [x] = x
mk xs = List.intercalate ", " (init xs <> ["and " <> last xs])
