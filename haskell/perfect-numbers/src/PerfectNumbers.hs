module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise = pure $ case sum (factors n) `compare` n of
    LT -> Deficient
    EQ -> Perfect
    GT -> Abundant

factors :: Integral n => n -> [n]
factors n = filter (`divides` n) [1..n `div` 2]

divides :: Integral n => n -> n -> Bool
a `divides` b = b `mod` a == 0
