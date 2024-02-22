{-# language LambdaCase #-}
module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Nil | Node { _prev :: IORef (Node a) , _val :: a , _next :: IORef (Node a) }
data Deque a = Deque { _last :: IORef (Node a) , _first :: IORef (Node a) }
  -- = Nil
  -- | Deque
  --   { _prev :: IORef (Deque a)
  --   , _value :: a
  --   , _next :: IORef (Deque a)
  --   }

nil :: IO (IORef (Node a))
nil = newIORef Nil

mkDeque :: IO (Deque a)
mkDeque = do
  r <- nil
  pure $ Deque r r

pop :: Deque a -> IO (Maybe a)
pop (Deque _ f) = do
  readIORef f >>= \case
    Nil -> pure Nothing
    Node p v _-> do
      p' <- readIORef p
      case p' of
        Nil -> pure ()
        Node _ _ n -> do
          writeIORef n Nil
      writeIORef f p'
      pure $ Just v

push :: Deque a -> a -> IO ()
push (Deque _ f) = do
  readIORef f >>= \case
    Nil -> do
      writeIORef f _

shift :: Deque a -> IO (Maybe a)
shift deque = error "You need to implement this function."

unshift :: Deque a -> a -> IO ()
unshift deque x = error "You need to implement this function."
