{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# options_ghc -Wno-unused-matches #-}
module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data Tree a = Tip | Branch (Tree a) a (Tree a) deriving (Show)

instance Ord a => Eq (Tree a) where
  a == b = a `isSubsetOf` b && b `isSubsetOf` a

type Set a = Tree a

delete :: Ord a => a -> Set a -> Set a
delete x = \case
  Tip -> Tip
  Branch l y r -> case x `compare` y of
    LT -> delete x l `union` r
    EQ -> l `union` r
    GT -> l `union` delete x r

difference :: Set a -> Set a -> Set a
difference setA setB = error "You need to implement this function."

empty :: Set a
empty = Tip

fromList :: Ord a => [a] -> Set a
fromList = \case
  [] -> Tip
  (x:xs) -> insert x $ fromList xs

singleton :: a -> Set a
singleton x = Branch Tip x Tip

insert :: Ord a => a -> Set a -> Set a
insert x = \case
  Tip -> singleton x
  Branch l y r -> case x `compare` y of
    LT -> Branch (insert x l) y r
    EQ -> Branch l y r
    GT -> Branch l y (insert x r)

intersection :: Ord a => Set a -> Set a -> Set a
Tip `intersection` _ = Tip
_ `intersection` Tip = Tip
Branch lx x rx `intersection` b@(Branch ly y ry) = case x `compare` y of
  LT -> (Branch lx x Tip `intersection` ly) `union` (rx `intersection` Branch Tip y ry)
  EQ -> Branch (lx `intersection` ly) x (rx `intersection` ry)
  GT -> (lx `intersection` Branch ly y Tip) `union` (Branch Tip x rx `intersection` rx)

isDisjointFrom :: Ord a => Set a -> Set a -> Bool
isDisjointFrom a b = case a of
  Tip -> True
  Branch l x r -> isDisjointFrom l b && isDisjointFrom r b && not (member x b)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf a b = case a of
  Tip -> True
  Branch l x r -> isSubsetOf l b && isSubsetOf r b && member x b

member :: Ord a => a -> Set a -> Bool
member x = \case
  Tip -> False
  Branch l y r -> case x `compare` y of
    LT -> member x l
    EQ -> True
    GT -> member x r

null :: Set a -> Bool
null = \case
  Tip -> True
  Branch{} -> False

size :: Set a -> Int
size = go 0
  where
  go n = \case
    Tip -> n
    Branch l _ r -> go (go (succ n) l) r

toList :: Set a -> [a]
toList = go []
  where
  go acc = \case
    Tip -> acc
    Branch l x r -> go (x : go acc r) l

-- `union` doesn't balance the trees. The tree with the larger root
-- simply becomes the new root.
union :: Ord a => Set a -> Set a -> Set a
Tip `union` b = b
a `union` Tip = a
Branch lx x rx `union` Branch ly y ry = case x `compare` y of
  LT -> rx `union` Branch (Branch lx x Tip `union` ly) y ry
  EQ -> Branch (lx `union` ly) x (rx `union` ry)
  GT -> ry `union` Branch (lx `union` Branch ly y Tip) x rx
