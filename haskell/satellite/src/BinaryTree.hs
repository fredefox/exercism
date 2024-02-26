{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
module BinaryTree (BinaryTree(..)) where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

deriving stock instance Functor BinaryTree
instance Applicative BinaryTree where
  pure x = Branch Leaf x Leaf
  _ <*> Leaf = Leaf
  Leaf <*> _ = Leaf
  Branch fl f fr <*> Branch l x r = Branch (fl <*> l) (f x) (fr <*> r)
