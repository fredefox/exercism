{-# language LambdaCase #-}
module Single where

import Data.IORef

data Node a = Nil | Cons a (List a)
  
newtype List a = List (IORef (Node a))

mkList :: IO (List a)
mkList = List <$> newIORef Nil

pop :: List a -> IO (Maybe a)
pop (List r) = go <$> readIORef r
  where
  go = \case { Nil -> Nothing ; Cons v _ -> pure v }

push :: a -> List a -> IO ()
push v (List r) = do
  n <- readIORef r
  w <- List <$> newIORef n
  let n' = Cons v w
  writeIORef r n'
