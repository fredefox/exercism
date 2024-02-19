{-# language Haskell2010 #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
-- {-# options_ghc -Wno-all #-}
module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), Forest)
import Control.Applicative
import Data.Maybe
import Data.Tree

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV = solve

subtreeC
  :: forall f a
  . Alternative f => Eq a
  => (Forest a -> Forest a) -> a -> Tree a -> f (Tree a)
subtreeC k a (Node x xs)
  | a == x = pure $ Node x (k xs)
  | otherwise
    = listToAlternative
    $ catMaybes
    $ fmap (subtreeC k a) xs

remove :: Alternative f => Eq a => a -> Tree a -> f (Tree a)
remove a (Node x xs)
  | x == a    = empty
  | otherwise = pure $ Node x $ foldMap (remove a) xs

invert :: forall f a . Alternative f => Eq a => (Tree a -> Tree a) -> a -> Tree a -> f (Tree a)
invert k a (Node x xs)
  | a == x = pure $ k (Node x xs)
  | otherwise = invertF id r a xs
  where
  r :: Forest a -> Forest a
  r = (\fs -> [k $ Node x fs])

invertF :: Alternative f => Eq a => (Forest a -> Forest a) -> (Forest a -> Forest a) -> a -> Forest a -> f (Tree a)
invertF _ _ _ [] = empty
invertF l r a (Node x xs:fs)
  | a == x = pure $ (Node x (r (l xs <> fs)))
  | otherwise = invertF l'' r a xs -- <|> invertF l' r a fs
  where
  l' = l . (Node x xs :)
  l'' = \fs -> ((Node x (l fs) : []))

-- solve a t = k
--   where
--   m :: Forest a
--   m = invert id a t
--   k :: f (Tree a)
--   k = subtreeC (m <>) a t

-- solve
--   :: forall f a
--   . Alternative f => Eq a
--   => a -> Tree a -> f (Tree a)
-- solve a t = k
--   where
--   m :: Forest a
--   m = remove a t
--   k :: f (Tree a)
--   k = subtreeC (m <>) a t

listToAlternative :: Alternative f => [a] -> f a
listToAlternative = \case
  [] -> empty
  (x:_) -> pure x

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween = undefined


nested = Node "level-0" [Node "level-1" [Node "level-2" [leaf "x"]]]

nested' = Node "x" [Node "level-1" [Node "level-2" [leaf "level-0"]]]

leaf = pure


flat = Node "root"
           [ leaf "a"
           , leaf "b"
           , leaf "x"
           , leaf "c"
           ]

flat' = Node "x"
            [ Node "root"
                  [ leaf "a"
                  , leaf "b"
                  , leaf "c"
                  ]
            ]

kids = Node "root"
           [ Node "x"
                 [ leaf "kid-0"
                 , leaf "kid-1"
                 ]
           ]

kids' = Node "x"
            [ leaf "kid-0"
            , leaf "kid-1"
            , leaf "root"
            ]

invertL :: ([a] -> [a]) -> [a] -> [a]
invertL k [] = k []
invertL k (x:xs) = invertL k' xs
  where
  k' = (x:) . k

invertF' :: forall f a . Alternative f => Eq a => a -> Forest a -> (Forest a -> Forest a) -> f (Tree a)
invertF' _ [] _ = empty
invertF' a (x:xs) k = invertT a x k' <|> invertF' a xs k''
  where
  k' :: Forest a -> Forest a
  k' fs = k (xs<>fs)
  k'' :: Forest a -> Forest a
  -- k'' fs = k $ (x:fs)
  k'' = k . (x:)

invertT :: forall f a . Alternative f => Eq a => a -> Tree a -> (Forest a -> Forest a) -> f (Tree a)
invertT a (Node x xs) k
  | a == x = pure $ Node x $ k xs
  | otherwise = invertF' @f a xs k'
  where
  k' :: Forest a -> Forest a
  -- k' fs = Node x [] : k fs
  k' fs = [Node x $ k fs]

solve
  :: forall f a
  . Alternative f => Eq a
  => a -> Tree a -> f (Tree a)
solve a t = invertT a t id
