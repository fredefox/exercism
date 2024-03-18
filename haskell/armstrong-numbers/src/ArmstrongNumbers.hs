module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum ((^ m) <$> ds)
  where
  ds = toDigits 10 n
  m = length ds

toDigits :: Integral n => n -> n -> [n]
toDigits beta = go []
  where
  go acc 0 = acc
  go acc n = go (b : acc) a
    where
    (a, b) = n `divMod` beta
