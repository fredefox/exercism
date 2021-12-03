{-# Language LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}
module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day(..), fromGregorian)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

instance Num Weekday where
  a + b = toEnum $ fromEnum a + fromEnum b
  a - b = toEnum $ fromEnum a - fromEnum b
  fromInteger = toEnum . fromInteger

instance Enum Weekday where
  toEnum i =
    case mod i 7 of
      0 -> Sunday
      1 -> Monday
      2 -> Tuesday
      3 -> Wednesday
      4 -> Thursday
      5 -> Friday
      _ -> Saturday
  fromEnum = \case
    Sunday -> 0
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    _ -> 6

dayOfWeek :: Day -> Weekday
dayOfWeek (ModifiedJulianDay d) = fromInteger $ d + 3

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

instance Num Day where
  ModifiedJulianDay a + ModifiedJulianDay b = ModifiedJulianDay (a + b)
  ModifiedJulianDay a - ModifiedJulianDay b = ModifiedJulianDay (a - b)
  fromInteger = toEnum . fromInteger

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  First -> first + toEnum diffFirst
  Second -> first + toEnum diffFirst + 7
  Third -> first + toEnum diffFirst + 14
  Fourth -> first + toEnum diffFirst + 21
  Last -> last' - toEnum diffLast
  Teenth -> teenth + toEnum diffTeenth
  where
  first = fromGregorian year month 1
  last' = fromGregorian year month 31
  teenth = fromGregorian year month 13
  -- Days from 1st to the first specified weekday
  diffFirst = fromEnum (weekday - dayOfWeek first)
  diffLast = fromEnum (dayOfWeek last' - weekday)
  diffTeenth = fromEnum (weekday - dayOfWeek teenth)
