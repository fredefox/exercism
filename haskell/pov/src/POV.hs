{-# language Haskell2010 #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module POV (fromPOV, tracePathBetween, invert, trace, path) where

import Data.Tree (Tree(Node), Forest)
import Control.Applicative
import Control.Monad

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV a = invert (== a)

invert
  :: Alternative f
  => (a -> Bool) -> Tree a -> f (Tree a)
invert = invertTree id

invertTree
  :: forall f a
  . Alternative f
  => (Forest a -> Forest a) -> (a -> Bool) -> Tree a -> f (Tree a)
invertTree k p (Node x xs) = here <|> there
  where
  here :: f (Tree a)
  here = t <$ guard (p x)
  t :: Tree a
  t = Node x $ (xs <> k [])
  there :: f (Tree a)
  there = invertForest @f k' p xs
  k' :: Forest a -> Forest a
  k' = pure . Node x . k

invertForest
  :: forall f a
  . Alternative f
  => (Forest a -> Forest a) -> (a -> Bool) -> Forest a -> f (Tree a)
invertForest _ _ [] = empty
invertForest k p (x:xs) = invertTree k' p x <|> invertForest k'' p xs
  where
  k' :: Forest a -> Forest a
  k' = (<> k xs)
  k'' :: Forest a -> Forest a
  k'' = k . (x:)

path :: forall f a . Alternative f => (a -> Bool) -> Tree a -> f [a]
path p (Node x xs) = here <|> there
  where
  here = [x] <$ guard (p x)
  there = fmap (x:) $ asum $ path p <$> xs

trace :: Alternative f => Monad f => (a -> Bool) -> (a -> Bool) -> Tree a -> f [a]
trace p q t = invert p t >>= path q

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween a b = trace (== a) (== b)
