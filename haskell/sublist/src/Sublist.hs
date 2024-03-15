module Sublist (sublist) where

import Data.List

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs `p` ys && ys `p` xs = Just EQ
  | xs `p` ys = Just LT
  | ys `p` xs = Just GT
  | otherwise = Nothing
  where
  p = isSublist

isSublist :: Eq a => [a] -> [a] -> Bool
xs@(_:_) `isSublist` ys@(_:yss)
  = xs `isPrefixOf` ys || xs `isSublist` yss
xs `isSublist` ys = xs `isPrefixOf` ys
