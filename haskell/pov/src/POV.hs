{-# language Haskell2010 #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), Forest)
import Control.Applicative

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV = invert id

invertF :: forall f a . Alternative f => Eq a => (Forest a -> Forest a) -> a -> Forest a -> f (Tree a)
invertF _ _ [] = empty
invertF k a (x:xs) = invert k' a x <|> invertF k'' a xs
  where
  k' :: Forest a -> Forest a
  k' = (<> k xs)
  k'' :: Forest a -> Forest a
  k'' = k . (x:)

invert :: forall f a . Alternative f => Eq a => (Forest a -> Forest a) -> a -> Tree a -> f (Tree a)
invert k a (Node x xs)
  | a == x = pure $ Node x $ (xs <> k [])
  | otherwise = invertF @f k' a xs
  where
  k' :: Forest a -> Forest a
  k' = pure . Node x . k

path :: forall f a . Alternative f => Eq a => a -> Tree a -> f [a]
path a (Node x xs)
  | a == x = pure [a]
  | otherwise = fmap (x:) $ asum $ path @f a <$> xs

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween a b t = do
  t' <- fromPOV a t
  path b t'
