{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language TupleSections #-}
{-# options_ghc -Wall #-}
module Bowling (score, BowlingError(..)) where

import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

data BowlingError
  = IncompleteGame
  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score xs = run xs eval

type Eval a = ExceptT BowlingError (ReaderT (Array Int Int) (State (Int, Int))) a

data Frame = Strike | Spare Int | Open Int Int

deriving stock instance Show Frame

getIndex :: Eval Int
getIndex = gets fst

incrIndex :: Eval ()
incrIndex = modify (\(a, b) -> (succ a, b))

getFrameIx :: Eval Int
getFrameIx = gets snd

incrFrameIx :: Eval ()
incrFrameIx = modify (\(a, b) -> (a, succ b))

invalidThrow :: Int -> Int -> Eval a
invalidThrow ix w = do
  throwError $ InvalidRoll ix w

returnThrow :: Int -> Int -> Eval Int
returnThrow ix w
  | w < 0 || w > 10 = invalidThrow ix w
  | otherwise = pure w

lookupV :: Array Int Int -> Int -> Eval Int
lookupV a n = case a !? n of
    Nothing -> throwError IncompleteGame
    Just w -> returnThrow n w

getNextThrow :: Eval (Int, Int)
getNextThrow = do
  ix <- getIndex
  incrIndex
  a <- ask
  (,ix) <$> lookupV a ix

peekThrow :: Int -> Eval Int
peekThrow d = do
  n <- getIndex
  a <- ask
  lookupV a (n + d)

(!?) :: Ix ix => Array ix e -> ix -> Maybe e
a !? ix
  | ix `inBounds` a = pure $ a Array.! ix
  | otherwise = Nothing

inBounds :: Ix ix => ix -> Array ix e -> Bool
ix `inBounds` a = Array.bounds a `Array.inRange` ix

getNextFrame :: Eval Frame
getNextFrame = do
  (n, _) <- getNextThrow
  incrFrameIx
  if n == 10
  then pure Strike
  else do
    (m, ix) <- getNextThrow
    if
      | n + m > 10 -> invalidThrow ix m
      | n + m == 10 -> pure $ Spare n
      | otherwise -> pure $ Open n m

eval :: Eval Int
eval = do
  f <- getNextFrame
  w <- scoreFrame f
  n <- getFrameIx
  (w +) <$> if n < 10
  then eval
  else 0 <$ ensureFinal

-- tryError :: MonadError e m => m a -> m (Either e a)
-- tryError m = m' `catchError` h
--   where
--   m' = Right <$> m
--   h = pure . Left

ensureFinal :: Eval ()
ensureFinal = do
  m <- tryError getNextThrow
  case m of
    Left{} -> pure ()
    Right (w, ix) -> invalidThrow ix w
  
scoreFrame :: Frame -> Eval Int
scoreFrame = \case
  Strike -> do
    x0 <- peekThrow 0
    x1 <- peekThrow 1
    ix <- getIndex
    when (x0 /= 10 && x0 + x1 > 10) $ invalidThrow (succ ix) x1
    pure $ sum [10, x0, x1]
  Spare{} -> do
    x0 <- peekThrow 0
    pure $ 10 + x0
  Open x0 x1 -> pure $ x0 + x1

run :: [Int] -> Eval a -> Either BowlingError a
run xs = (`evalState` (0,0)) . (`runReaderT` listArray xs) . runExceptT

listArray :: [a] -> Array Int a
listArray xs = Array.listArray (0, pred $ length xs) xs
