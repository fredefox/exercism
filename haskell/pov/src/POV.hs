{-# language Haskell2010 #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), Forest)
import Control.Applicative
import Control.Monad

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV a = invert id (== a)

invert :: forall f a . Alternative f => (Forest a -> Forest a) -> (a -> Bool) -> Tree a -> f (Tree a)
invert k p (Node x xs) = here <|> there
  where
  here :: f (Tree a)
  here = t <$ guard (p x)
  t :: Tree a
  t = Node x $ (xs <> k [])
  there :: f (Tree a)
  there = invertForest @f k' p xs
  k' :: Forest a -> Forest a
  k' = pure . Node x . k

invertForest :: forall f a . Alternative f => (Forest a -> Forest a) -> (a -> Bool) -> Forest a -> f (Tree a)
invertForest _ _ [] = empty
invertForest k a (x:xs) = invert k' a x <|> invertForest k'' a xs
  where
  k' :: Forest a -> Forest a
  k' = (<> k xs)
  k'' :: Forest a -> Forest a
  k'' = k . (x:)

path :: forall f a . Alternative f => Eq a => a -> Tree a -> f [a]
path a (Node x xs)
  | a == x = pure [a]
  | otherwise = fmap (x:) $ asum $ path @f a <$> xs

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween a b t = fromPOV a t >>= path b
