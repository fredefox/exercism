{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Satellite (treeFromTraversals) where

import Data.List
import BinaryTree (BinaryTree(..))
import Control.Applicative

treeFromTraversals :: Show a => Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Nothing
treeFromTraversals xs _ | duplicates xs = Nothing
treeFromTraversals xs ys  = listToAlternative $ mkTree xs ys

mkTree :: Eq a => [a] -> [a] -> [BinaryTree a]
mkTree [] [] = pure Leaf
mkTree (x:xs) ys = do
  (yl, yr) <- cleave (== x) ys
  let (xl, xr) = span (`elem` yl) xs
  Branch <$> mkTree xl yl <*> pure x <*> mkTree xr yr
mkTree _ _ = empty

cleave :: (a -> Bool) -> [a] -> [([a], [a])]
cleave p xs = foldMap go $ splits p xs
  where
  go (a, _:b) = pure (a, b)
  go _ = error "IMPOSSIBLE"

splits :: (a -> Bool) -> [a] -> [([a], [a])]
splits p xs = filter p' $ zip (inits xs) (tails xs)
  where
  p' (_, []) = False
  p' (_, (x:_)) = p x

duplicates :: Ord a => [a] -> Bool
duplicates xs = length xs /= length (nub xs)

listToAlternative :: Alternative f => [a] -> f a
listToAlternative [] = empty
listToAlternative (x:_) = pure x
