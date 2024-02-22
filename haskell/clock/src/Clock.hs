module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock h' m'
  where
  (a, m') = m `divMod` 60
  h' = (a + h) `mod` 24

toString :: Clock -> String
toString (Clock h m) = s h <> ":" <> s m
  where
  s n | n < 10 = "0" <> show n
      | otherwise = show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock h m) = fromHourMin (dh + h) (dm + m)
