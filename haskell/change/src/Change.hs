{-# language CPP #-}
{-# language ScopedTypeVariables #-}
module Change (findFewestCoins, solve) where

#ifdef NAIVE
import Control.Applicative
#endif
import Data.Array (Array, Ix, (!))
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Function
import Data.Maybe
import Control.Monad

#ifdef NAIVE
findFewestCoins :: Integral n => n -> [n] -> Maybe [n]
findFewestCoins n xs = naive n (reverse xs)
#else
findFewestCoins :: Ix n => Integral n => n -> [n] -> Maybe [n]
findFewestCoins n xs = join $ solve n xs !? n
#endif

#ifdef NAIVE
naive :: Integral n => n -> [n] -> Maybe [n]
naive 0 _ = pure []
naive _ [] = empty
naive n xs@(x:xss)
  | x > n = naive n xss
  | otherwise = (x:) <$> naive (n - x) xs
#endif

array :: Ix i => (i, i) -> (i -> e) -> Array i e
array b f = Array.array b $ fmap f' $ Array.range b
  where
  f' ix = (ix, f ix)

solve :: forall n . Integral n => Ix n => n -> [n] -> Array n (Maybe [n])
solve n xs = a
  where
  b = (0, n)
  a :: Array n (Maybe [n])
  a = array b f
  f :: n -> (Maybe [n])
  f ix
    | ix < 0 = Nothing
    | ix == 0 = Just []
    | otherwise = case catMaybes $ fmap step xs of
        [] -> Nothing
        ys -> pure $ minimumOn length ys
    where
    step :: n -> Maybe [n]
    step x = fmap (x:) $ join (a !? (ix - x))

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? ix
  | inBounds ix a = pure $ a ! ix
  | otherwise = Nothing

inBounds :: Ix i => i -> Array i e -> Bool
inBounds ix a = Array.bounds a `Array.inRange` ix

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn p = List.minimumBy (compare `on` p)
