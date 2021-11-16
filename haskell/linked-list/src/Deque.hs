{-# LANGUAGE LambdaCase #-}
module Deque (Deque, mkDeque, pop, push, shift, unshift, toList) where

import Data.IORef

newtype Deque a = Deque (IORef (Maybe (Node a)))

data Node a = Node (Deque a) a (Deque a)

mkDeque :: IO (Deque a)
mkDeque = Deque <$> newIORef Nothing

pop :: Deque a -> IO (Maybe a)
pop (Deque r) = do
  readIORef r >>= \case
    Nothing -> pure Nothing
    Just (Node _ v (Deque r')) -> do
      writeIORef r' Nothing
      pure $ pure v

node :: a -> IO (Node a)
node v = Node <$> mkDeque <*> pure v <*> mkDeque

push :: Deque a -> a -> IO ()
push (Deque r) v = do
  n@(Node (Deque d0) _ _) <- node v
  writeIORef r $ pure n
  readIORef r >>= \case
    Nothing -> pure ()
    Just m@(Node _ _ (Deque d1)) -> do
      writeIORef d0 (pure m)
      writeIORef d1 (pure n)

unshift :: Deque a -> a -> IO ()
unshift (Deque r) v = do
  n@(Node _ _ (Deque d0)) <- node v
  readIORef r >>= \case
    Nothing -> do
      writeIORef r $ pure n
    Just m@(Node (Deque d1) _ _) -> do
      writeIORef d0 (pure m)
      writeIORef d1 (pure n)

shift :: Deque a -> IO (Maybe a)
shift (Deque r) = do
  readIORef r >>= \case
    Nothing -> pure Nothing
    Just (Node (Deque r') v _) -> do
      writeIORef r' Nothing
      pure $ pure v


toList :: Deque a -> IO [a]
toList (Deque r) = do
  readIORef r >>= \case
    Nothing -> pure mempty
    Just (Node _ x r') -> (x :) <$> toList r'
