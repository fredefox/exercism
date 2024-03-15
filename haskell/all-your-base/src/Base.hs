module Base (Error(..), rebase) where

import Control.Monad

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase xs = do
  when (inputBase  <= 1) (Left InvalidInputBase)
  when (outputBase <= 1) (Left InvalidOutputBase)
  case filter ((||) <$> (< 0) <*> (>= inputBase)) xs of
    [] -> pure ()
    (x:_) -> Left $ InvalidDigit x
  pure $ toDigits outputBase $ fromDigits inputBase xs

toDigits :: Integral n => n -> n -> [n]
toDigits beta = go []
  where
  go acc 0 = acc
  go acc n = go (b : acc) a
    where
    (a, b) = n `divMod` beta

fromDigits :: Integral n => n -> [n] -> n
fromDigits beta = foldl (\acc x -> acc * beta + x) 0
