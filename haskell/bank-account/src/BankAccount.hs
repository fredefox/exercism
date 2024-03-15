module BankAccount
  ( BankAccount
  , closeAccount
  , getBalance
  , incrementBalance
  , openAccount
  ) where

import GHC.Conc
import Control.Monad (void)

type BankAccount = TVar (Maybe Integer)

closeAccount :: BankAccount -> IO ()
closeAccount = void . modifyTVar (const Nothing)

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readTVarIO

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance m n = modifyTVar (fmap (+ n)) m

modifyTVar :: (a -> a) -> TVar a -> IO a
modifyTVar f v = atomically $ do
  w <- readTVar v
  let w' = f w
  w' <$ writeTVar v w'

openAccount :: IO BankAccount
openAccount = newTVarIO (Just 0)
