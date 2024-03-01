{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}
module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    , array
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad

data Array ix a = Array ix ix (Vector a)
deriving stock instance (Eq ix, Eq a) => Eq (Array ix a)
deriving stock instance (Show ix, Show a) => Show (Array ix a)

type Matrix a = Array (Int, Int) a

class Ix a where
  range :: a -> a -> [a]
  unsafeIndex :: a -> a -> a -> Int
  inRange :: a -> a -> a -> Bool

size :: Ix a => a -> a -> Int
size a b = unsafeIndex a b b

index :: Ix a => a -> a -> a -> Int
index a b ix
  | inRange a b ix = unsafeIndex a b ix
  | otherwise = error "index out of range"

instance Ix Int where
  a `range` b = [a..pred b]
  unsafeIndex a _ ix = ix - a
  inRange a b ix = a <= ix && ix < b

instance (Ix a, Ix b) => Ix (a, b) where
  (a0, a1) `range` (b0, b1) = (,) <$> a0 `range` b0 <*> a1 `range` b1
  unsafeIndex (a0, b0) (a1, b1) (aix, bix) = n * n0 + n1
    where
    n0 = unsafeIndex a0 a1 aix
    n = size b0 b1
    n1 = unsafeIndex b0 b1 bix
  inRange (a0, b0) (a1, b1) (aix, bix) = inRange a0 a1 aix && inRange b0 b1 bix

cols :: Matrix a -> Int
cols (Array (_, c0) (_, c1) _) = c1 - c0

column :: forall a . Int -> Matrix a -> Vector a
column (pred -> ixc) = column' ixc

column' :: forall ix a . Eq ix => Ix ix => ix -> Array (ix, ix) a -> Vector a
column' ix a@(Array l u _) = Vector.fromList $ (a !) <$> ixs
  where
  ixs :: [(ix, ix)]
  ixs = filter (\(_, j) -> j == ix) $ range l u

(!) :: Ix ix => Array ix a -> ix -> a
Array l u v ! ix = v Vector.! index l u ix

flatten :: Matrix a -> Vector a
flatten (Array _ _ v) = v

array :: forall ix a . Ix ix => ix -> ix -> (ix -> a) -> Array ix a
array l u f = Array l u $ Vector.fromList $ f <$> range @ix l u

fromList :: [[a]] -> Matrix a
fromList [] = Array (0, 0) (0, 0) mempty
fromList xs@(x:_) = Array (0, 0) (r, c) (Vector.fromList (join xs))
  where
  r = length xs
  c = length x

fromString :: Read a => String -> Matrix a
fromString = fromList . fmap (fmap read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Array _ _ v) = Array (0, 0) (r, c) v

row :: forall a . Int -> Matrix a -> Vector a
row (pred -> ixr) = row' ixr

row' :: forall ix a . Eq ix => Ix ix => ix -> Array (ix, ix) a -> Vector a
row' ix a@(Array l u _) = Vector.fromList $ (a !) <$> ixs
  where
  ixs :: [(ix, ix)]
  ixs = filter (\(i, _) -> i == ix) $ range l u

rows :: Matrix a -> Int
rows (Array (r0, _) (r1, _) _) = r1 - r0

shape :: Matrix a -> (Int, Int)
shape (Array (r0, c0) (r1, c1) _) = (r1 - r0, c1 - c0)

transpose :: forall a . Matrix a -> Matrix a
transpose a@(Array (r0, c0) (r1, c1) _) = array (c0, r0) (c1, r1) f
  where
  f :: (Int, Int) -> a
  f (r, c) = a ! (c, r)
