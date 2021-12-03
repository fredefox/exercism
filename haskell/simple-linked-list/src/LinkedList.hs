{-# LANGUAGE LambdaCase #-}
module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Cons a _) = a

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil = \case
  Nil -> True
  _   -> False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next (Cons _ as) = as

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
  go acc = \case
    Nil -> acc
    Cons a as -> go (Cons a acc) as

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons a as) = a : toList as
