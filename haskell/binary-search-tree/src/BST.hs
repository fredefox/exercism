{-# language LambdaCase, NamedFieldPuns, StandaloneDeriving, DerivingStrategies #-}
module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Node
  { value :: a
  , left  :: BST a
  , right :: BST a
  } | Empty

deriving stock instance Show a => Show (BST a)
deriving stock instance Eq a => Eq (BST a)

bstLeft :: BST a -> Maybe (BST a)
bstLeft = \case
  Empty -> Nothing
  Node{left} -> Just left

bstRight :: BST a -> Maybe (BST a)
bstRight = \case
  Empty -> Nothing
  Node{right} -> Just right

bstValue :: BST a -> Maybe a
bstValue = \case
  Empty -> Nothing
  Node{value} -> Just value

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert a = \case
  Empty      -> singleton a
  Node v l r -> case a <= v of
    True -> Node v (insert a l) r
    False -> Node v l (insert a r)

singleton :: a -> BST a
singleton a = Node a Empty Empty

toList :: BST a -> [a]
toList = \case
  Empty -> mempty
  Node v l r -> toList l <> [v] <> toList r
