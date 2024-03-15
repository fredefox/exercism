{-# language GHC2021 #-}
module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char

transform :: Map a String -> Map Char a
transform = Map.fromList . foldMap step . Map.toList
  where
  step (x, xs) = (,x) <$> (fmap toLower xs)
