{-# language ScopedTypeVariables #-}
{-# language PartialTypeSignatures #-}
module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Cont

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals = undefined

solve
  :: forall a . Ord a
  => [a] -> [a] -> Maybe (BinaryTree a, [a])
solve xs ys = evalCont $ solveC xs ys

solveC :: Eq a => [a] -> [a] -> Cont (Maybe (BinaryTree a, [a])) (Maybe (BinaryTree a, [a]))
solveC [] [] = pure $ Just (Leaf, [])
solveC (x:xs) (y:ys) | x == y = callCC $ \ret -> do
  mr <- solveC xs ys
  let
    there :: Cont _ _
    there = do
      m0 <- solveC (x:xs) ys
      m1 <- solveC xs (y:ys)
      let mb = do
            (l, ys') <- m0
            (r, ys'') <- m1
            pure (Branch l x r, ys')
      r <- ret mb
      pure $ (Branch r
      case mb of
        Nothing -> _
        Just (Branch l x r, ys) -> do
          r <- ret $ _ r
          _ r
      -- pure $ do
      --   (Branch l x r, ys) <- mb
      --   pure $ (Branch l x (_ret r), ys)
  _ (pure $ here mr) there
  where
  here mr = do
    (r, ys') <- mr
    pure $ (Branch Leaf x r, ys')
    
-- callCC $ \ret -> do
--   case (xs, ys) of
--     ([], []) -> pure $ Just (Leaf, [])
--     (x:xs, y:ys)
--       | x == y -> do
--           solveC xs ys
-- (x:xs) (y:ys) = here <|> there
--   where
--   here :: Maybe (BinaryTree a, [a])
--   here = do
--     guard (x == y)
--     (r, ys') <- solve xs ys
--     pure (Branch Leaf x r, ys')
--   there :: Maybe (BinaryTree a, [a])
--   there = do
--     (l, ys') <- solve (x:xs) ys
--     (r, ys'') <- solve xs (y:ys')
--     pure (Branch l x r, ys'')
-- solve [] [] = pure (Leaf, [])
-- solve _ ys = pure (Leaf, ys) -- XXX

-- slv :: forall a . Ord a => [a] -> [a] -> Maybe (BinaryTree a)
-- slv xs ys = evalCont c
--   where
--   c :: Cont (Maybe (BinaryTree a)) (Maybe (BinaryTree a))
--   c = reset d
--   d :: forall b . Cont b (Maybe (BinaryTree a))
--   d = shift $ \k -> pure _
--     where
--     go :: (Maybe (BinaryTree a) -> b) -> b
--     go k = case (xs, ys) of
--       (x:xs', y:ys') -> k (here <|> there)
--         where
--         here :: Maybe (BinaryTree a)
--         here = do
--           guard (x == y)
--           r <- slv xs' ys'
--           pure $ Branch Leaf x r
--         there :: Maybe (BinaryTree a)
--         there = do
--           solve (x:xs) ys
--           _
--       ([], []) -> k (pure Leaf)
