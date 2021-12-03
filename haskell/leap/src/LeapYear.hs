module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear n
  | go 400    = True
  | go 100    = False
  | go 4      = True
  | otherwise = False
  where
  go m = n `mod` m == 0
